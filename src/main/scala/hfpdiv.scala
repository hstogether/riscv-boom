//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// FDiv/FSqrt Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2016 Feb 5


package boom

import Chisel._
import config.Parameters

import tile.FPConstants._


// TODO : FIXED ME -- JECY

class UOPCodeHFDivDecoder extends Module
{
  val io = IO(new Bundle {
    val uopc = Bits(INPUT, UOPC_SZ)
    val sigs = new tile.FPUCtrlSigs().asOutput
  })

   val N = BitPat("b0")
   val Y = BitPat("b1")
   val X = BitPat("b?")

   val decoder = rocket.DecodeLogic(io.uopc,
      // Note: not all of these signals are used or necessary, but we're
      // constrained by the need to fit the rocket.FPU units' ctrl signals.
      //                                                    swap13          fromhfp
      //                                                    | swap32        | tohfp
      //                            cmd                     | | single      | | fastpipe
      //                            |            ldst       | | | half      | | | fma
      //                            |            | wen      | | | | fromint | | | |  div
      //                            |            | | ren1   | | | | | toint | | | |  | sqrt
      //                            |            | | | ren2 | | | | | | fromfp| | |  | | round
      //                            |            | | | | ren3 | | | | | | tofp| | |  | | | wflags
      //                            |            | | | | |  | | | | | | | | | | | |  | | | |
      /* Default */            List(FCMD_X,      X,X,X,X,X, X,X,X,Y,X,X,X,X,X,X,X,X, X,X,X,X),
      Array(
         BitPat(uopFDIV_H)  -> List(FCMD_DIV,    X,X,Y,Y,X, X,X,N,Y,X,X,X,X,X,X,X,X, Y,N,Y,Y),
         BitPat(uopFSQRT_H) -> List(FCMD_SQRT,   X,X,Y,N,X, X,X,N,Y,X,X,X,X,X,X,X,X, N,Y,Y,Y)
      ))

   val s = io.sigs
   val sigs = Seq(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.single, s.half, s.fromint, s.toint, s.fromfp, s.tofp,
                  s.fromhfp, s.tohfp, s.fastpipe, s.fma, s.div, s.sqrt, s.round,
                  s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}


// fdiv/fsqrt is douple-precision. Must upconvert inputs and downconvert outputs
// as necessary.  Must wait till killed uop finishes before we're ready again.
// fdiv/fsqrt unit uses an unstable FIFO interface, and thus we must spend a
// cycle buffering up an uop to provide slack between the issue queue and the
// fdiv/fsqrt unit.  FDivUnit inherents directly from FunctionalUnit, because
// UnpipelinedFunctionalUnit can only handle 1 inflight uop, whereas FDivUnit
// contains up to 2 inflight uops due to the need to buffer the input as the
// fdiv unit uses an unstable FIFO interface.
// TODO extend UnpipelinedFunctionalUnit to handle a >1 uops inflight.
class HFDivSqrtUnit(implicit p: Parameters) extends FunctionalUnit(is_pipelined = false
                                                                 , num_stages = 1
                                                                 , num_bypass_stages = 0
                                                                 , data_width = 68)(p)
{
   //--------------------------------------
   // buffer inputs and upconvert as needed

   // provide a one-entry queue to store incoming uops while waiting for the fdiv/fsqrt unit to become available.
   val r_buffer_val = Reg(init = Bool(false))
   val r_buffer_req = Reg(new FuncUnitReq(data_width=68))
   val r_buffer_fin = Reg(new tile.HFPInput)

   val fdiv_decoder = Module(new UOPCodeHFDivDecoder)
   fdiv_decoder.io.uopc := io.req.bits.uop.uopc

   // handle branch kill on queued entry
   r_buffer_val := !IsKilledByBranch(io.brinfo, r_buffer_req.uop) && !io.req.bits.kill && r_buffer_val
   r_buffer_req.uop.br_mask := GetNewBrMask(io.brinfo, r_buffer_req.uop)

   // handle incoming uop, including upconversion as needed, and push back if our input queue is already occupied
   io.req.ready := !r_buffer_val

   when (io.req.valid && !IsKilledByBranch(io.brinfo, io.req.bits.uop) && !io.req.bits.kill)
   {
      r_buffer_val := Bool(true)
      r_buffer_req := io.req.bits
      r_buffer_req.uop.br_mask := GetNewBrMask(io.brinfo, io.req.bits.uop)
      r_buffer_fin := fdiv_decoder.io.sigs
      r_buffer_fin.rm := io.fcsr_rm
      r_buffer_fin.typ := UInt(0) // unused for fdivsqrt
      r_buffer_fin.in1 := io.req.bits.rs1_data
      r_buffer_fin.in2 := io.req.bits.rs2_data
   }

   assert (!(r_buffer_val && io.req.valid), "[fdiv] a request is incoming while the buffer is already full.")

   //-----------
   // fdiv/fsqrt

   //val divsqrt = Module(new hardfloat.DivSqrtRecFN_small(5,11,0))

   //val r_divsqrt_val = Reg(init = Bool(false))  // inflight uop?
   //val r_divsqrt_killed = Reg(Bool())           // has inflight uop been killed?
   //val r_divsqrt_fin = Reg(new tile.HFPInput)
   //val r_divsqrt_uop = Reg(new MicroOp)

   //// Need to buffer output until RF writeport is available.
   //val output_buffer_available = Wire(Bool())


   val divsqrt0= Module(new hardfloat.DivSqrtRecFN_small(5,11,0))
   val r_divsqrt0_val = Reg(init = Bool(false))  // inflight uop?
   val r_divsqrt0_killed = Reg(Bool())           // has inflight uop been killed?
   val r_divsqrt0_fin = Reg(new tile.HFPInput)
   val r_divsqrt0_uop = Reg(new MicroOp)
   val output0_buffer_available = Wire(Bool())

   val divsqrt1 = Module(new hardfloat.DivSqrtRecFN_small(5,11,0))
   val r_divsqrt1_val = Reg(init = Bool(false))  // inflight uop?
   val r_divsqrt1_killed = Reg(Bool())           // has inflight uop been killed?
   val r_divsqrt1_fin = Reg(new tile.HFPInput)
   val r_divsqrt1_uop = Reg(new MicroOp)
   val output1_buffer_available = Wire(Bool())

   val divsqrt2 = Module(new hardfloat.DivSqrtRecFN_small(5,11,0))
   val r_divsqrt2_val = Reg(init = Bool(false))  // inflight uop?
   val r_divsqrt2_killed = Reg(Bool())           // has inflight uop been killed?
   val r_divsqrt2_fin = Reg(new tile.HFPInput)
   val r_divsqrt2_uop = Reg(new MicroOp)
   val output2_buffer_available = Wire(Bool())

   val divsqrt3 = Module(new hardfloat.DivSqrtRecFN_small(5,11,0))
   val r_divsqrt3_val = Reg(init = Bool(false))  // inflight uop?
   val r_divsqrt3_killed = Reg(Bool())           // has inflight uop been killed?
   val r_divsqrt3_fin = Reg(new tile.HFPInput)
   val r_divsqrt3_uop = Reg(new MicroOp)
   val output3_buffer_available = Wire(Bool())


//   val may_fire_input =
//      r_buffer_val &&
//      (r_buffer_fin.div || r_buffer_fin.sqrt) &&
//      !r_divsqrt_val &&
//      output_buffer_available

   val may_fire_input =
      r_buffer_val &&
      (r_buffer_fin.div || r_buffer_fin.sqrt) &&
      !r_divsqrt0_val && !r_divsqrt1_val && !r_divsqrt2_val && !r_divsqrt3_val
      output0_buffer_available && output1_buffer_available &&
      output2_buffer_available && output3_buffer_available



   //val divsqrt_ready = divsqrt.io.inReady
   //divsqrt.io.inValid := may_fire_input // must be setup early
   //divsqrt.io.sqrtOp := r_buffer_fin.sqrt
   //divsqrt.io.a := r_buffer_fin.in1
   //divsqrt.io.b := Mux(divsqrt.io.sqrtOp, r_buffer_fin.in1, r_buffer_fin.in2)
   //divsqrt.io.roundingMode := r_buffer_fin.rm

   //r_divsqrt_killed := r_divsqrt_killed || IsKilledByBranch(io.brinfo, r_divsqrt_uop) || io.req.bits.kill
   //r_divsqrt_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt_uop)

   val divsqrt0_ready = divsqrt0.io.inReady
   divsqrt0.io.inValid := may_fire_input // must be setup early
   divsqrt0.io.sqrtOp := r_buffer_fin.sqrt
   divsqrt0.io.a := r_buffer_fin.in1
   divsqrt0.io.b := Mux(divsqrt0.io.sqrtOp, r_buffer_fin.in1, r_buffer_fin.in2)
   divsqrt0.io.roundingMode := r_buffer_fin.rm
   r_divsqrt0_killed := r_divsqrt0_killed || IsKilledByBranch(io.brinfo, r_divsqrt0_uop) || io.req.bits.kill
   r_divsqrt0_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt0_uop)

 
   val divsqrt1_ready = divsqrt1.io.inReady
   val divsqrt1_a     = Cat(Fill(51,r_buffer_fin.in1(33)),r_buffer_fin.in1(33,17))
   val divsqrt1_b     = Cat(Fill(51,r_buffer_fin.in2(33)),r_buffer_fin.in2(33,17))
   divsqrt1.io.inValid := may_fire_input // must be setup early
   divsqrt1.io.sqrtOp := r_buffer_fin.sqrt
   divsqrt1.io.a := divsqrt1_a
   divsqrt1.io.b := Mux(divsqrt1.io.sqrtOp, divsqrt1_a, divsqrt1_b)
   divsqrt1.io.roundingMode := r_buffer_fin.rm
   r_divsqrt1_killed := r_divsqrt1_killed || IsKilledByBranch(io.brinfo, r_divsqrt1_uop) || io.req.bits.kill
   r_divsqrt1_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt1_uop)

 
   val divsqrt2_ready = divsqrt2.io.inReady
   val divsqrt2_a     = Cat(Fill(51,r_buffer_fin.in1(50)),r_buffer_fin.in1(50,34))
   val divsqrt2_b     = Cat(Fill(51,r_buffer_fin.in2(50)),r_buffer_fin.in2(50,34))
   divsqrt2.io.inValid := may_fire_input // must be setup early
   divsqrt2.io.sqrtOp := r_buffer_fin.sqrt
   divsqrt2.io.a := divsqrt2_a
   divsqrt2.io.b := Mux(divsqrt2.io.sqrtOp, divsqrt2_a, divsqrt2_b)
   divsqrt2.io.roundingMode := r_buffer_fin.rm
   r_divsqrt2_killed := r_divsqrt2_killed || IsKilledByBranch(io.brinfo, r_divsqrt2_uop) || io.req.bits.kill
   r_divsqrt2_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt2_uop)

 
   val divsqrt3_ready = divsqrt3.io.inReady
   val divsqrt3_a     = Cat(Fill(51,r_buffer_fin.in1(67)),r_buffer_fin.in1(67,51))
   val divsqrt3_b     = Cat(Fill(51,r_buffer_fin.in2(67)),r_buffer_fin.in2(67,51))
   divsqrt3.io.inValid := may_fire_input // must be setup early
   divsqrt3.io.sqrtOp := r_buffer_fin.sqrt
   divsqrt3.io.a := divsqrt3_a
   divsqrt3.io.b := Mux(divsqrt0.io.sqrtOp, divsqrt3_a, divsqrt3_b)
   divsqrt3.io.roundingMode := r_buffer_fin.rm
   r_divsqrt3_killed := r_divsqrt3_killed || IsKilledByBranch(io.brinfo, r_divsqrt3_uop) || io.req.bits.kill
   r_divsqrt3_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt3_uop)

 
   when (may_fire_input && divsqrt0_ready && divsqrt1_ready && divsqrt2_ready && divsqrt3_ready)
   //when (may_fire_input && divsqrt_ready)
   {
      // Remove entry from the input buffer.
      // We don't have time to kill divsqrt request so must track if killed on entry.
      r_buffer_val := Bool(false)

      r_divsqrt0_val := Bool(true)
      r_divsqrt0_fin := r_buffer_fin
      r_divsqrt0_uop := r_buffer_req.uop
      r_divsqrt0_killed := IsKilledByBranch(io.brinfo, r_buffer_req.uop) || io.req.bits.kill
      r_divsqrt0_uop.br_mask := GetNewBrMask(io.brinfo, r_buffer_req.uop)

      r_divsqrt1_val := Bool(true)
      r_divsqrt1_fin := r_buffer_fin
      r_divsqrt1_uop := r_buffer_req.uop
      r_divsqrt1_killed := IsKilledByBranch(io.brinfo, r_buffer_req.uop) || io.req.bits.kill
      r_divsqrt1_uop.br_mask := GetNewBrMask(io.brinfo, r_buffer_req.uop)

      r_divsqrt2_val := Bool(true)
      r_divsqrt2_fin := r_buffer_fin
      r_divsqrt2_uop := r_buffer_req.uop
      r_divsqrt2_killed := IsKilledByBranch(io.brinfo, r_buffer_req.uop) || io.req.bits.kill
      r_divsqrt2_uop.br_mask := GetNewBrMask(io.brinfo, r_buffer_req.uop)

      r_divsqrt3_val := Bool(true)
      r_divsqrt3_fin := r_buffer_fin
      r_divsqrt3_uop := r_buffer_req.uop
      r_divsqrt3_killed := IsKilledByBranch(io.brinfo, r_buffer_req.uop) || io.req.bits.kill
      r_divsqrt3_uop.br_mask := GetNewBrMask(io.brinfo, r_buffer_req.uop)

   }

   //-----------------------------------------
   // buffer output and down-convert as needed

//   val r_out_val = Reg(init=Bool(false))
//   val r_out_uop = Reg(new MicroOp)
//   val r_out_flags_double = Reg(Bits())
//   val r_out_wdata_double = Reg(Bits())
//
//   output_buffer_available := !r_out_val
//
//   r_out_uop.br_mask := GetNewBrMask(io.brinfo, r_out_uop)
//
//   when (io.resp.ready || IsKilledByBranch(io.brinfo, r_out_uop) || io.req.bits.kill)
//   {
//      r_out_val := Bool(false)
//   }
//   when (divsqrt.io.outValid_div || divsqrt.io.outValid_sqrt)
//   {
//      r_divsqrt_val := Bool(false)
//
//      r_out_val := !r_divsqrt_killed && !IsKilledByBranch(io.brinfo, r_divsqrt_uop) && !io.req.bits.kill
//      r_out_uop := r_divsqrt_uop
//      r_out_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt_uop)
//      r_out_wdata_double := Cat(Fill(51,divsqrt.io.out(16)),divsqrt.io.out)
//      r_out_flags_double := divsqrt.io.exceptionFlags
//
//      assert (r_divsqrt_val, "[fdiv] a response is being generated for no request.")
//   }
//
//   assert (!(r_out_val && (divsqrt.io.outValid_div || divsqrt.io.outValid_sqrt)),
//      "[fdiv] Buffered output being overwritten by another output from the fdiv/fsqrt unit.")
//


   val r_out0_val = Reg(init=Bool(false))
   val r_out0_uop = Reg(new MicroOp)
   val r_out0_flags = Reg(Bits())
   val r_out0_wdata = Reg(Bits())
   output0_buffer_available := !r_out0_val
   r_out0_uop.br_mask := GetNewBrMask(io.brinfo, r_out0_uop)

   val r_out1_val = Reg(init=Bool(false))
   val r_out1_uop = Reg(new MicroOp)
   val r_out1_flags = Reg(Bits())
   val r_out1_wdata = Reg(Bits())
   output1_buffer_available := !r_out1_val
   r_out1_uop.br_mask := GetNewBrMask(io.brinfo, r_out1_uop)

   val r_out2_val = Reg(init=Bool(false))
   val r_out2_uop = Reg(new MicroOp)
   val r_out2_flags = Reg(Bits())
   val r_out2_wdata = Reg(Bits())
   output2_buffer_available := !r_out2_val
   r_out2_uop.br_mask := GetNewBrMask(io.brinfo, r_out2_uop)

   val r_out3_val = Reg(init=Bool(false))
   val r_out3_uop = Reg(new MicroOp)
   val r_out3_flags = Reg(Bits())
   val r_out3_wdata = Reg(Bits())
   output3_buffer_available := !r_out3_val
   r_out3_uop.br_mask := GetNewBrMask(io.brinfo, r_out3_uop)


   when (io.resp.ready && r_out0_val && r_out1_val && r_out2_val && r_out3_val
         || IsKilledByBranch(io.brinfo, r_out0_uop) || io.req.bits.kill)
   {
      r_out0_val := Bool(false)
   }
   when (divsqrt0.io.outValid_div || divsqrt0.io.outValid_sqrt)
   {
      r_divsqrt0_val := Bool(false)

      r_out0_val := !r_divsqrt0_killed && !IsKilledByBranch(io.brinfo, r_divsqrt0_uop) && !io.req.bits.kill
      r_out0_uop := r_divsqrt0_uop
      r_out0_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt0_uop)
      r_out0_wdata := divsqrt0.io.out
      r_out0_flags := divsqrt0.io.exceptionFlags

      assert (r_divsqrt0_val, "[fdiv0] a response is being generated for no request.")
   }
   assert (!(r_out0_val && (divsqrt0.io.outValid_div || divsqrt0.io.outValid_sqrt)),
      "[fdiv0] Buffered output being overwritten by another output from the fdiv/fsqrt unit.")


   when (io.resp.ready && r_out0_val && r_out1_val && r_out2_val && r_out3_val
         || IsKilledByBranch(io.brinfo, r_out1_uop) || io.req.bits.kill)
   {
      r_out1_val := Bool(false)
   }
   when (divsqrt1.io.outValid_div || divsqrt1.io.outValid_sqrt)
   {
      r_divsqrt1_val := Bool(false)

      r_out1_val := !r_divsqrt1_killed && !IsKilledByBranch(io.brinfo, r_divsqrt1_uop) && !io.req.bits.kill
      r_out1_uop := r_divsqrt1_uop
      r_out1_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt1_uop)
      r_out1_wdata := divsqrt1.io.out
      r_out1_flags := divsqrt1.io.exceptionFlags

      assert (r_divsqrt1_val, "[fdiv1] a response is being generated for no request.")
   }
   assert (!(r_out1_val && (divsqrt1.io.outValid_div || divsqrt1.io.outValid_sqrt)),
      "[fdiv] Buffered output being overwritten by another output from the fdiv/fsqrt unit.")


   when (io.resp.ready && r_out0_val && r_out1_val && r_out2_val && r_out3_val
         || IsKilledByBranch(io.brinfo, r_out2_uop) || io.req.bits.kill)
   {
      r_out2_val := Bool(false)
   }
   when (divsqrt2.io.outValid_div || divsqrt2.io.outValid_sqrt)
   {
      r_divsqrt2_val := Bool(false)

      r_out2_val := !r_divsqrt2_killed && !IsKilledByBranch(io.brinfo, r_divsqrt2_uop) && !io.req.bits.kill
      r_out2_uop := r_divsqrt2_uop
      r_out2_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt2_uop)
      r_out2_wdata := divsqrt2.io.out
      r_out2_flags := divsqrt2.io.exceptionFlags

      assert (r_divsqrt2_val, "[fdiv2] a response is being generated for no request.")
   }
   assert (!(r_out2_val && (divsqrt2.io.outValid_div || divsqrt2.io.outValid_sqrt)),
      "[fdiv2] Buffered output being overwritten by another output from the fdiv/fsqrt unit.")


   when (io.resp.ready && r_out0_val && r_out1_val && r_out2_val && r_out3_val
         || IsKilledByBranch(io.brinfo, r_out3_uop) || io.req.bits.kill)
   {
      r_out3_val := Bool(false)
   }
   when (divsqrt3.io.outValid_div || divsqrt3.io.outValid_sqrt)
   {
      r_divsqrt3_val := Bool(false)

      r_out3_val := !r_divsqrt3_killed && !IsKilledByBranch(io.brinfo, r_divsqrt3_uop) && !io.req.bits.kill
      r_out3_uop := r_divsqrt3_uop
      r_out3_uop.br_mask := GetNewBrMask(io.brinfo, r_divsqrt3_uop)
      r_out3_wdata := divsqrt3.io.out // 17-bit
      r_out3_flags := divsqrt3.io.exceptionFlags

      assert (r_divsqrt3_val, "[fdiv3] a response is being generated for no request.")
   }
   assert (!(r_out3_val && (divsqrt3.io.outValid_div || divsqrt3.io.outValid_sqrt)),
      "[fdiv3] Buffered output being overwritten by another output from the fdiv/fsqrt unit.")

   val maxN = Cat(Bits(1),Bits(0),Fill(14,Bits(1)))
   val zero = Cat(Fill(16,Bits(0)))
 
   val data3 = Mux(r_out3_wdata(15,13) === UInt(6), Cat(r_out3_wdata(16),maxN),
                   Mux(r_out3_wdata(15,13) === UInt(0), Cat(r_out3_wdata(16), zero), r_out3_wdata))
   val data2 = Mux(r_out2_wdata(15,13) === UInt(6), Cat(r_out2_wdata(16),maxN),
                   Mux(r_out2_wdata(15,13) === UInt(0), Cat(r_out2_wdata(16), zero), r_out2_wdata))
   val data1 = Mux(r_out1_wdata(15,13) === UInt(6), Cat(r_out1_wdata(16),maxN),
                   Mux(r_out1_wdata(15,13) === UInt(0), Cat(r_out1_wdata(16), zero), r_out1_wdata))
   val data0 = Mux(r_out0_wdata(15,13) === UInt(6), Cat(r_out0_wdata(16),maxN),
                   Mux(r_out0_wdata(15,13) === UInt(0), Cat(r_out0_wdata(16), zero), r_out0_wdata))

   io.resp.valid := r_out0_val && r_out1_val && r_out2_val && r_out3_val && !IsKilledByBranch(io.brinfo, r_out0_uop)
   io.resp.bits.uop := r_out0_uop
   io.resp.bits.data := Cat(data3,data2,data1,data0)
   io.resp.bits.fflags.valid := io.resp.valid
   io.resp.bits.fflags.bits.uop := r_out0_uop
   io.resp.bits.fflags.bits.uop.br_mask := GetNewBrMask(io.brinfo, r_out0_uop)
   io.resp.bits.fflags.bits.flags := r_out0_flags | r_out1_flags | r_out2_flags | r_out3_flags

//
//   io.resp.valid := r_out_val && !IsKilledByBranch(io.brinfo, r_out_uop)
//   io.resp.bits.uop := r_out_uop
//   io.resp.bits.data := r_out_wdata_double
//   io.resp.bits.fflags.valid := io.resp.valid
//   io.resp.bits.fflags.bits.uop := r_out_uop
//   io.resp.bits.fflags.bits.uop.br_mask := GetNewBrMask(io.brinfo, r_out_uop)
//   io.resp.bits.fflags.bits.flags := r_out_flags_double
//
   if(DEBUG_PRINTF_HFDIV){
      printf("HFPDIV-Start----------------------------------------------------------------------------------\n")
      printf("  In:\n")
      printf("    io.req.bits.uop.valid=[%d]    io.req.bits.uop.uopc=[%d]    fdiv_decoder.io.sigs.cmd=[%d]    io.req.bits.rs1_data=[%x]    io.req.bits.rs2_data=[%x]\n",
                  io.req.bits.uop.valid.asUInt, io.req.bits.uop.uopc,        fdiv_decoder.io.sigs.cmd,        io.req.bits.rs1_data,        io.req.bits.rs2_data);
      printf("    r_buffer_val=[%d]    r_buffer_req.uop.uopc=[%d]    r_buffer_fin.in1=[%x]    r_buffer_fin.in2=[%x]\n\n",
                  r_buffer_val.asUInt, r_buffer_req.uop.uopc,        r_buffer_fin.in1,        r_buffer_fin.in2)
      printf("    divsqrt0.io.inValid=[%d]    divsqrt0.io.sqrtOp=[%d]    divsqrt0.io.a=[%x]    divsqrt0.io.b=[%x]\n",
                  divsqrt0.io.inValid.asUInt, divsqrt0.io.sqrtOp.asUInt, divsqrt0.io.a,        divsqrt0.io.b)
      printf("    divsqrt1.io.inValid=[%d]    divsqrt1.io.sqrtOp=[%d]    divsqrt1.io.a=[%x]    divsqrt1.io.b=[%x]\n",
                  divsqrt1.io.inValid.asUInt, divsqrt1.io.sqrtOp.asUInt, divsqrt1.io.a,        divsqrt1.io.b)
      printf("    divsqrt2.io.inValid=[%d]    divsqrt2.io.sqrtOp=[%d]    divsqrt2.io.a=[%x]    divsqrt2.io.b=[%x]\n",
                  divsqrt2.io.inValid.asUInt, divsqrt2.io.sqrtOp.asUInt, divsqrt2.io.a,        divsqrt2.io.b)
      printf("    divsqrt3.io.inValid=[%d]    divsqrt3.io.sqrtOp=[%d]    divsqrt3.io.a=[%x]    divsqrt3.io.b=[%x]\n\n",
                  divsqrt3.io.inValid.asUInt, divsqrt3.io.sqrtOp.asUInt, divsqrt3.io.a,        divsqrt3.io.b)
 
      printf("  Out:\n")
      printf("    divsqrt0.io.outValid_div=[%d]    divsqrt0.io.outValid_sqrt=[%d]    divsqrt0.io.out=[%x][%d]\n",
                  divsqrt0.io.outValid_div.asUInt, divsqrt0.io.outValid_sqrt.asUInt, divsqrt0.io.out,divsqrt0.io.exceptionFlags)
      printf("    divsqrt1.io.outValid_div=[%d]    divsqrt1.io.outValid_sqrt=[%d]    divsqrt1.io.out=[%x][%d]\n",
                  divsqrt1.io.outValid_div.asUInt, divsqrt1.io.outValid_sqrt.asUInt, divsqrt1.io.out,divsqrt1.io.exceptionFlags)
      printf("    divsqrt2.io.outValid_div=[%d]    divsqrt2.io.outValid_sqrt=[%d]    divsqrt2.io.out=[%x][%d]\n",
                  divsqrt2.io.outValid_div.asUInt, divsqrt2.io.outValid_sqrt.asUInt, divsqrt2.io.out,divsqrt2.io.exceptionFlags)
      printf("    divsqrt3.io.outValid_div=[%d]    divsqrt3.io.outValid_sqrt=[%d]    divsqrt3.io.out=[%x][%d]\n\n",
                  divsqrt3.io.outValid_div.asUInt, divsqrt3.io.outValid_sqrt.asUInt, divsqrt3.io.out,divsqrt3.io.exceptionFlags)
        
      printf("    r_out0_val=[%d]    r_out0_uop.uopc=[%d]    r_out0_wdata=[%x][%d]\n",
                  r_out0_val.asUInt, r_out0_uop.uopc,        r_out0_wdata,r_out0_flags)
      printf("    r_out1_val=[%d]    r_out1_uop.uopc=[%d]    r_out1_wdata=[%x][%d]\n",
                  r_out1_val.asUInt, r_out1_uop.uopc,        r_out1_wdata,r_out1_flags)
      printf("    r_out2_val=[%d]    r_out2_uop.uopc=[%d]    r_out2_wdata=[%x][%d]\n",
                  r_out2_val.asUInt, r_out2_uop.uopc,        r_out2_wdata,r_out2_flags)
      printf("    r_out3_val=[%d]    r_out3_uop.uopc=[%d]    r_out3_wdata=[%x][%d]\n",
                  r_out3_val.asUInt, r_out3_uop.uopc,        r_out3_wdata,r_out3_flags)
      printf("    io.resp.valid=[%d]    io.resp.bits.uop.uopc=[%d]    io.resp.bits.data=[%x][%d]\n",
                  io.resp.valid.asUInt, io.resp.bits.uop.uopc,        io.resp.bits.data,io.resp.bits.fflags.bits.flags)
      printf("HFPDIV-End------------------------------------------------------------------------------------\n")
   }
}

