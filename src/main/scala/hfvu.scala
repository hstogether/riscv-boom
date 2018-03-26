//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

// Jecy 20171130
// FP16 unit
// TODO：寻找关于hfp_val的传递过程

package boom
{
   // Note: fdiv, fsqrt unsupported.
   // Note: (this HFPU currently only supports fixed latency ops)

import Chisel._
import config.Parameters

import tile.FPConstants._
import tile.FPUCtrlSigs

import util.uintToBitPat


// TODO get rid of this decoder and move into the Decode stage? Or the RRd stage?
// most of these signals are already created, just need to be translated
// to the Rocket FPU-speak
class UOPCodeHFVUDecoder extends Module
{
  val io = IO(new Bundle {
    val uopc = Bits(INPUT, UOPC_SZ)
    val sigs = new FPUCtrlSigs().asOutput
  })

   // TODO change N,Y,X to BitPat("b1"), BitPat("b0"), and BitPat("b?")
   val N = Bool(false)
   val Y = Bool(true)
   val X = Bool(false)

   val default: List[BitPat] = List(FCMD_X,    X,X,X,X,X, X,X,X,X,X,X,X,X,X,X,X,X, X,X,X,X)
   val table: Array[(BitPat, List[BitPat])] =
      //                                                  swap12          fromhfp
      //                                                  | swap32        | tohfp
      //                          cmd                     | | single      | | fastpipe
      //                          |            ldst       | | | half      | | | fma
      //                          |            | wen      | | | | fromint | | | |  div
      //                          |            | | ren1   | | | | | toint | | | |  | sqrt
      //                          |            | | | ren2 | | | | | | fromfp| | |  | | round
      //                          |            | | | | ren3 | | | | | | tofp| | |  | | | wflags
      //                          |            | | | | |  | | | | | | | | | | | |  | | | |
      Array(
      BitPat(uopRS1_H)    -> List(FCMD_HFS,    X,X,Y,N,N, N,X,N,Y,N,N,N,N,Y,Y,Y,N, N,N,N,N),
      BitPat(uopRS2_H)    -> List(FCMD_HFS,    X,X,Y,N,N, N,X,N,Y,N,N,N,N,Y,Y,Y,N, N,N,N,N),
      BitPat(uopRS3_H)    -> List(FCMD_HFS,    X,X,Y,N,N, N,X,N,Y,N,N,N,N,Y,Y,Y,N, N,N,N,N),
      BitPat(uopFI0_H)    -> List(FCMD_HFS,    X,X,Y,N,N, N,X,N,Y,N,N,N,N,Y,Y,Y,N, N,N,N,N),
      BitPat(uopCR_H)     -> List(FCMD_HFS,    X,X,Y,N,N, N,X,N,Y,N,N,N,N,Y,Y,Y,N, N,N,N,N),
      BitPat(uopTR_H)     -> List(FCMD_HFS,    X,X,Y,N,N, N,X,N,Y,N,N,N,N,Y,Y,Y,N, N,N,N,N),

      BitPat(uopPAL_H)    -> List(FCMD_HFS,    X,X,Y,Y,Y, N,X,N,Y,N,N,N,N,Y,Y,Y,N, N,N,N,N),
      BitPat(uopPAH_H)    -> List(FCMD_HFS,    X,X,Y,Y,Y, N,X,N,Y,N,N,N,N,Y,Y,Y,N, N,N,N,N),

      BitPat(uopPMAX_H)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,Y,N,N,N,Y,Y,Y,Y,N, N,N,N,Y), // tohfp for max
      BitPat(uopPMIN_H)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,Y,N,N,N,N,Y,N,Y,N, N,N,N,Y),
      BitPat(uopPSUM_H)   -> List(FCMD_ADD,    X,X,Y,N,N, N,Y,N,Y,N,N,N,N,Y,Y,N,Y, N,N,Y,Y),
      BitPat(uopPAVE_H)   -> List(FCMD_ADD,    X,X,Y,N,N, N,Y,N,Y,N,Y,N,N,Y,Y,N,Y, N,N,Y,Y) // toint for ave
      )

   val decoder = rocket.DecodeLogic(io.uopc, default, table)

   val s = io.sigs
   val sigs = Seq(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.single, s.half, s.fromint, s.toint, s.fromfp, s.tofp,
                  s.fromhfp, s.tohfp, s.fastpipe, s.fma, s.div, s.sqrt, s.round,
                  s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}

class HFVU(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val req = new ValidIO(new FpuReq).flip
      val resp = new ValidIO(new ExeUnitResp(68))
   })

   // all FP units are padded out to the same latency for easy scheduling of the write port
   val hfvu_latency = hfmaLatency * 2
   val io_req = io.req.bits

   val hfv_decoder = Module(new UOPCodeHFVUDecoder)
   hfv_decoder.io.uopc:= io_req.uop.uopc // 微操作码，eg：uopADD_H
   val hfv_ctrl = hfv_decoder.io.sigs
   val hfv_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === Bits(7), io_req.fcsr_rm, ImmGenRm(io_req.uop.imm_packed)) // 选择舍入方式(111寄存器，其他指令中)

   val req = Wire(new tile.HFPInput)
   req := hfv_ctrl
   req.rm := hfv_rm
   req.in1 := io_req.rs1_data
   req.in2 := io_req.rs2_data
   req.in3 := io_req.rs3_data
   when (hfv_ctrl.swap23) { req.in3 := io_req.rs2_data }

   req.typ := ImmGenTyp(io_req.uop.imm_packed) // 获得当前指令的类型，00单精度；01双精度；10半精度

   val hfsu = Module(new HFSU())
   hfsu.io.req.valid := io.req.valid && hfv_ctrl.cmd === FCMD_HFS
   hfsu.io.req.bits  := req

   val hpru = Module(new HPRU())
   hpru.io.req.valid := io.req.valid && (hfv_ctrl.cmd === FCMD_MINMAX || hfv_ctrl.cmd === FCMD_ADD)
   hpru.io.req.bits  := req

   // Response (all HFP units have been padded out to the same latency)
   io.resp.valid := hfsu.io.res.valid ||
                    hpru.io.res.valid
   val hfvu_out   = Mux(hfsu.io.res.valid,  hfsu.io.res.bits,
                                            hpru.io.res.bits)

   io.resp.bits.data              := hfvu_out.data
   io.resp.bits.fflags.valid      := io.resp.valid
   io.resp.bits.fflags.bits.flags := hfvu_out.exc

   if(DEBUG_PRINTF_HFPU){
      printf("HFVU-Start--------------------------------------------------------------------------------------------\n")
      printf("hfvu_latency=[%d]\n", UInt(hfvu_latency))
      printf("io.req.bits.uop.uopc=[%d]    req.cmd=[%d]    req.rm=[%d]    req.rs1=[%x]    req.rs2=[%x]    req.rs3=[%x]\n",
              io_req.uop.uopc,             req.cmd,        req.rm,        io_req.rs1_data,io_req.rs2_data,io_req.rs3_data);
      printf("hfsu.io.req.valid=[%d]    hpru.io.in.valid=[%d]\n",
              hfsu.io.req.valid.asUInt, hpru.io.req.valid.asUInt)
      printf("hfsu.io.res.valid=[%d]  hpru_out.valid =[%d]\n",
              hfsu.io.res.valid.asUInt, hpru.io.res.valid.asUInt)
      printf("hfsu.io.res=[%x][%d]    hpruio.res=[%x][%d]\n",
              hfsu.io.res.bits.data, hfsu.io.res.bits.exc,   
              hpru.io.res.bits.data, hpru.io.res.bits.exc)
      printf("resp.data=[%x][%d]    resp.valid=[%d]\n",
              io.resp.bits.data, io.resp.bits.fflags.bits.flags, io.resp.valid.asUInt);
      printf("HFVU-End--------------------------------------------------------------------------------------------\n")
   }


// TODO why is this assertion failing?
//   assert (PopCount(Vec(ifpu.io.out, fpiu_out, fpmu.io.out, sfma.io.out, dfma.io.out).map(_.valid)) <= UInt(1),
//      "Multiple FPU units are firing requests.")
}

// 2 cycle
class HCMP(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle {
      val req = Valid(new tile.HFPInput).flip
      val res = Valid(new tile.HFPResult)
   }
   
   val req1 = Reg(new tile.HFPInput)
   val valid1 = io.req.valid
   when (io.req.valid) { req1 := io.req.bits }
   val in10 = req1.in1(16,0)
   val in11 = req1.in1(33,17)
   val in12 = req1.in1(50,34)
   val in13 = req1.in1(67,51)

   val req2 = Pipe(Reg(next=io.req.valid), io.req.bits, 2)
   val in20 = req2.bits.in1(16,0)
   val in21 = req2.bits.in1(33,17)
   val in22 = req2.bits.in1(50,34)
   val in23 = req2.bits.in1(67,51)

   // min/max
   val cmp0 = Module(new hardfloat.CompareRecFN(5,11))
   cmp0.io.a := in10
   cmp0.io.b := in11
   cmp0.io.signaling := UInt(1)

   val cmp1 = Module(new hardfloat.CompareRecFN(5,11))
   cmp1.io.a := in12
   cmp1.io.b := in13
   cmp1.io.signaling := UInt(1)

   val max1 = Reg(Bool())
   val in4 = Reg(Bits(17))
   val in5 = Reg(Bits(17))
   max1 := req1.tohfp
   in4 := Mux((max1 && cmp0.io.gt) ||
              (!max1 && (cmp0.io.lt || cmp0.io.eq)), in20,
               in21)
   in5 := Mux((max1 && cmp1.io.gt) ||
              (!max1 && (cmp1.io.lt || cmp1.io.eq)), in22,
               in23)
   val exc1 = Reg(Bits(5))
   exc1 := cmp0.io.exceptionFlags | cmp1.io.exceptionFlags

   val cmp2 = Module(new hardfloat.CompareRecFN(5,11))
   cmp2.io.a := in4
   cmp2.io.b := in5
   cmp2.io.signaling := UInt(1)

   val max2 = req2.bits.tohfp
   val out = Mux((max2 && cmp0.io.gt) ||
                 (!max2 && (cmp2.io.lt || cmp2.io.eq)), in4,
                  in5)
   val exc2 = exc1 | cmp2.io.exceptionFlags
   
   io.res.valid := req2.valid
   io.res.bits.exc := exc2
   io.res.bits.data := Cat(Fill(51,UInt(0)),out)

   if(DEBUG_PRINTF_HFPU){
      printf("HCMP-------------------------------------------------------------------\n")
      printf("hcmp-io.req.valid=[%d]    io.req.bits.in1=[%x]    io.req.bits.in2=[%x]\n",
                   io.req.valid.asUInt, io.req.bits.in1,        io.req.bits.in2)
      printf("  req1.valid=[%d]      req1.in1=[%x]      req1.in2=[%x]\n",
                valid1.asUInt,       req1.in1,          req1.in2)
      printf("  req2.valid=[%d]      req2.bits.in1=[%x]      req2.bits.in2=[%x]\n\n",
                req2.valid.asUInt,   req2.bits.in1,          req2.bits.in2)

      printf("in10=[%x]    in11=[%x]    in12=[%x]    in13=[%x]\n",
              in10,        in11,        in12,        in13)
      printf("cmp0.io.lt=[%d]    cmp0.io.eq=[%d]    cmp0.io.gt=[%d]\n",
              cmp0.io.lt.asUInt, cmp0.io.eq.asUInt, cmp0.io.gt.asUInt)
      printf("cmp1.io.lt=[%d]    cmp1.io.eq=[%d]    cmp1.io.gt=[%d]\n\n",
              cmp1.io.lt.asUInt, cmp1.io.eq.asUInt, cmp1.io.gt.asUInt)

      printf("in20=[%x]    in21=[%x]    in22=[%x]    in23=[%x]\n",
              in20,        in21,        in22,        in23)
      printf("max1=[%d]    in4=[%x]     in5=[%x]     exc1=[%d]\n",
              max1.asUInt, in4,         in5,         exc1)
      printf("cmp2.io.lt=[%d]    cmp2.io.eq=[%d]    cmp1.io.gt=[%d]\n\n",
              cmp2.io.lt.asUInt, cmp2.io.eq.asUInt, cmp2.io.gt.asUInt)

      printf("max2=[%d]    out=[%x]     exc2=[%d]\n",
              max2.asUInt, out,         exc2)
      printf("hcmp-io.res.valid=[%d]    io.res.bits.data=[%x]    io.res.bits.exc=[%d]\n",
                   io.res.valid.asUInt, io.res.bits.data,        io.res.bits.exc)
      printf("HCMP-------------------------------------------------------------------\n")
   }
}

// 2 cycle
class HSUM(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle {
      val req = Valid(new tile.HFPInput).flip
      val res = Valid(new tile.HFPResult)
   }
   
   val req1 = Reg(new tile.HFPInput)
   val valid1 = io.req.valid
   when (io.req.valid) { req1 := io.req.bits }
   val in0 = req1.in1(16,0)
   val in1 = req1.in1(33,17)
   val in2 = req1.in1(50,34)
   val in3 = req1.in1(67,51)
   val one = UInt(1) << 10

   val req2 = Pipe(Reg(next=io.req.valid), io.req.bits, 2)

   // min/max
   val hsum0 = Module(new hardfloat.MulAddRecFN(5,11))
   hsum0.io.op := Bits(0,2)
   hsum0.io.roundingMode := UInt(0)
   hsum0.io.a := in0
   hsum0.io.b := one
   hsum0.io.c := in1

   val hsum1 = Module(new hardfloat.MulAddRecFN(5,11))
   hsum1.io.op := Bits(0,2)
   hsum1.io.roundingMode := UInt(0)
   hsum1.io.a := in2
   hsum1.io.b := one
   hsum1.io.c := in3

   val maxN  = Cat(Fill(4,Bits(1)),Bits(0),Fill(10,Bits(1)))
   val hsum0_out = hsum0.io.out
   val hsum1_out = hsum1.io.out
   val ave = Reg(Bool())
   val in4 = Reg(Bits(17))
   val in5 = Reg(Bits(17))
   ave := req1.toint
   in4 := Mux(hsum0_out(15,14) === UInt(3) && hsum0_out(9,0) === UInt(0),
              Cat(hsum0_out(16),maxN), hsum0_out)
   in5 := Mux(hsum1_out(15,14) === UInt(3) && hsum1_out(9,0) === UInt(0),
              Cat(hsum1_out(16),maxN), hsum1_out)
   val exc1 = Reg(Bits(5))
   exc1 := hsum0.io.exceptionFlags | hsum1.io.exceptionFlags

   val hsum2 = Module(new hardfloat.MulAddRecFN(5,11))
   hsum2.io.op := Bits(0,2)
   hsum2.io.roundingMode := UInt(0)
   hsum2.io.a := in4
   hsum2.io.b := one
   hsum2.io.c := in5

   val hsum2_out = hsum2.io.out
   val hsum2_out_fN = hardfloat.fNFromRecFN(5, 11, hsum2_out(16,0))
   val hsum2_exp = Mux(hsum2_out_fN(14,10) > UInt(1), hsum2_out_fN(15,11) - UInt(2), UInt(1,5))
   val hsum2_fN = Cat(hsum2_out_fN(15), hsum2_exp, hsum2_out_fN(9,0))
   val hsum2_ave = hardfloat.recFNFromFN(5, 11, hsum2_fN)
   val hsum2_res = Mux(ave, Cat(Fill(51,UInt(0)),hsum2_ave), hsum2_out)
   val out  = Mux(hsum2_out(15,14) === UInt(3) && hsum2_out(9,0) === UInt(0),
                  Cat(hsum2_out(16),maxN), hsum2_res)
   val exc  = hsum2.io.exceptionFlags | exc1 

   io.res.valid := req2.valid
   io.res.bits.exc := exc
   io.res.bits.data := Cat(Fill(51,UInt(0)),out)

   if(DEBUG_PRINTF_HFPU){
      printf("HSUM-------------------------------------------------------------------------\n")
      printf("hsum-io.req.valid=[%d]    io.req.bits.in1=[%x]    io.req.bits.in2=[%x]\n",
                   io.req.valid.asUInt, io.req.bits.in1,        io.req.bits.in2)
      printf("  req1.valid=[%d]      req1.in1=[%x]      req1.in2=[%x]\n",
                valid1.asUInt,       req1.in1,          req1.in2)
      printf("  req2.valid=[%d]      req2.bits.in1=[%x]      req2.bits.in2=[%x]\n\n",
                req2.valid.asUInt,   req2.bits.in1,          req2.bits.in2)

      printf("in0=[%x]    in1=[%x]    in2=[%x]    in3=[%x]\n",
              in0,        in1,        in2,        in3)
      printf(" hsum0_out=[%x][%d]    hsum1_out=[%x][%d]\n\n",
               hsum0_out, hsum0.io.exceptionFlags,
               hsum1_out, hsum1.io.exceptionFlags)

      printf("maxN=[%x]    one=[%x]    in4=[%x]     in5=[%x]     exc1=[%d]\n",
              maxN.asUInt, one,        in4,         in5,         exc1)
      printf("hsum2_out=[%x][%d]\n\n",
              hsum2_out, hsum2.io.exceptionFlags)

      printf("hsum2_out_fN=[%x]    hsum2_exp=[%x]    hsum2_fN=[%x]\n",
              hsum2_out_fN,        hsum2_exp,        hsum2_fN)
      printf("hsum2_ave=[%x]    hsum2_res=[%x]    out=[%x]    exc=[%d]\n\n",
              hsum2_ave,        hsum2_res,        out,        exc)

      printf("hsum-io.res.valid=[%d]    io.res.bits.data=[%x]    io.res.bits.exc=[%d]\n",
                   io.res.valid.asUInt, io.res.bits.data,        io.res.bits.exc)
      printf("HSUM-------------------------------------------------------------------------\n")
   }
}


class HPRU()(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle {
      val req = Valid(new tile.HFPInput).flip
      val res = Valid(new tile.HFPResult)
   }
   val hfvu_latency = hfmaLatency * 2
   val hcmp =  Module(new HCMP())
   hcmp.io.req.valid := io.req.valid && io.req.bits.cmd === FCMD_MINMAX
   hcmp.io.req.bits := io.req


   val hsum = Module(new HSUM())
   hsum.io.req.valid := io.req.valid && io.req.bits.cmd === FCMD_ADD
   hsum.io.req.bits := io.req

   val valid = hcmp.io.res.valid || hsum.io.res.valid
   val out = Mux(hcmp.io.res.valid, hcmp.io.res, hsum.io.res)

   val out8   = Pipe(Reg(next=valid), out.bits, hfvu_latency-2)

   io.res.valid := out8.valid
   io.res.bits.data := out8.bits.data
   io.res.bits.exc := out8.bits.exc

   if(DEBUG_PRINTF_HFPU){
      printf("HPRU---------------------------------------------------------------------------------------------\n")
      printf("hpru-io.req.valid=[%d]    io.req.bits.in1=[%x]    io.req.bits.in2=[%x]    io.req.bits.in3=[%x]\n\n",
                   io.req.valid.asUInt, io.req.bits.in1,        io.req.bits.in2,        io.req.bits.in3)

      printf("hcmp-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
                   hcmp.io.req.valid.asUInt, hcmp.io.req.bits.in1, hcmp.io.req.bits.in2,  hcmp.io.req.bits.in3)  
      printf("hcmm-io.res.valid=[%d]    io.res.data=[%x]    io.res.exc=[%d]\n",
                   hcmp.io.res.valid.asUInt, hcmp.io.res.bits.data,   hcmp.io.res.bits.exc)
      printf("hsum-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
                   hsum.io.req.valid.asUInt, hsum.io.req.bits.in1, hsum.io.req.bits.in2,  hsum.io.req.bits.in3)  
      printf("hsum-io.res.valid=[%d]    io.res.data=[%x]    io.res.exc=[%d]\n\n",
                   hsum.io.res.valid.asUInt, hsum.io.res.bits.data,   hsum.io.res.bits.exc)

      printf("valid=[%d]    out=[%x][%d]\n", valid, out.bits.data, out.bits.exc)
      printf("hpru-io.res.valid=[%d]    io.res.bits.data=[%x][%d]\n",
                   io.res.valid.asUInt, io.res.bits.data, io.res.bits.exc)
      printf("HPRU---------------------------------------------------------------------------------------------\n")
   }
}

class HFSU()(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle {
      val req = Valid(new tile.HFPInput).flip
      val res = Valid(new tile.HFPResult)
   }
   val hfvu_latency = hfmaLatency * 2

   val rm = io.req.bits.rm
   val in0 = io.req.bits.in1(16,0)
   val in1 = io.req.bits.in1(33,17) 
   val in2 = io.req.bits.in1(50,34)
   val in3 = io.req.bits.in1(67,51)

   val out0 = Wire(Bits(17))
   val out1 = Wire(Bits(17))
   val out2 = Wire(Bits(17))
   val out3 = Wire(Bits(17))

   val sel0 = rm === UInt(0)
   val sel1 = rm === UInt(1)
   val sel2 = rm === UInt(2)
   val sel3 = rm === UInt(3)
   val sel4 = rm === UInt(4)
   val sel5 = rm === UInt(5)
   val sel6 = rm === UInt(6)
   val sel7 = rm === UInt(7)
   assert( !(io.req.valid && (sel0 || sel7)), "HFSU input rm eq 0 or 7.")

   out0 := Mux(sel4, in0,
           Mux(sel1 || sel5, in1,
           Mux(sel2 || sel6, in2,
           Mux(sel3, in3,
               UInt(0)))))
   out1 := Mux(sel3 || sel4 || sel5 || sel6, in0,
           Mux(sel1, in1,
           Mux(sel2, in3,
               UInt(0))))
   out2 := Mux(sel2 || sel4, in0,
           Mux(sel3 || sel6, in1,
           Mux(sel1 || sel5, in3,
               UInt(0))))
   out3 := Mux(sel1 || sel4, in0,
           Mux(sel2, in1,
           Mux(sel3 || sel5, in2,
           Mux(sel6, in3,
               UInt(0)))))

   when(io.req.bits.ren2 && io.req.bits.ren3){
      out0 := Mux(sel1, io.req.bits.in1(16,0),
              Mux(sel2, io.req.bits.in3(16,0),
                  UInt(0)))
      out1 := Mux(sel1, io.req.bits.in2(16,0),
              Mux(sel2, io.req.bits.in3(33,17),
                  UInt(0)))
      out2 := Mux(sel1, io.req.bits.in3(50,34),
              Mux(sel2, io.req.bits.in1(16,0),
                  UInt(0)))
      out3 := Mux(sel1, io.req.bits.in3(67,51),
              Mux(sel2, io.req.bits.in2(16,0),
                  UInt(0)))
   }

   val out = Wire(new tile.HFPResult)
   out.data := Cat(out3, out2, out1, out0)
   out.exc := UInt(0)
   val hfsu_out = Pipe(Reg(next=io.req.valid),
                       out, hfvu_latency)

   io.res.valid     := hfsu_out.valid
   io.res.bits.exc  := hfsu_out.bits.exc
   io.res.bits.data := hfsu_out.bits.data

   if(DEBUG_PRINTF_HFPU){
     printf("HFSU---------------------------------------------------------------------------------------------\n")
     printf("hfsu-io.req.valid=[%d]    io.req.bits.in1=[%x]    io.req.bits.in2=[%x]    io.req.bits.in3=[%x]    io.req.bits.rm=[%d]\n",
                  io.req.valid.asUInt, io.req.bits.in1,        io.req.bits.in2,        io.req.bits.in3,        io.req.bits.rm)
     printf("io.req.bits.ren1=[%d]    io.req.bits.ren2=[%d]    io.req.bits.ren3=[%d]\n\n",
             io.req.bits.ren1.asUInt, io.req.bits.ren2.asUInt, io.req.bits.ren3.asUInt)

     printf("in0=[%x]    in1=[%x]    in2=[%x]    in3=[%x]\n",
             in0,        in1,        in3,        in3)
     printf("sel0=[%d]    sel1=[%d]    sel2=[%d]    sel3=[%d]    sel4=[%d]    sel5=[%d]    sel6=[%d]    sel7=[%d]\n\n",
             sel0.asUInt, sel1.asUInt, sel2.asUInt, sel3.asUInt, sel4.asUInt, sel5.asUInt, sel6.asUInt, sel7.asUInt)

     printf("out0=[%x]    out1=[%x]    out2=[%x]    out3=[%x]    out=[%x]    exc=[%d]\n",
             out0,        out1,        out2,        out3,        out.data,   out.exc)
     printf("io.res.valid=[%d]    io.res.bits.data=[%x][%d]\n",
             io.res.valid.asUInt, io.res.bits.data, io.res.bits.exc)
     printf("HFSU---------------------------------------------------------------------------------------------\n")
   }
}

//
//class HPRU(hfvu_latency: Int = hfmaLatency * 2)(implicit p: Parameters) extends BoomModule()(p)
//{
//   val io = new Bundle {
//      val req = Valid(new tile.HFPInput).flip
//      val res = Valid(new tile.HFPResult)
//   }
//
//   val in0 = cat(Fill(51,UInt(0),io.req.bits.in1(16,0)))
//   val in1 = cat(Fill(51,UInt(0),io.req.bits.in1(33,17)))
//   val in2 = cat(Fill(51,UInt(0),io.req.bits.in1(50,34)))
//   val in3 = cat(Fill(51,UInt(0),io.req.bits.in1(67,51)))
//   val maxN = Cat(Fill(4,Bits(1)),Bits(0),Fill(10,Bits(1)))
//   val zero = Cat(Fill(57,Bits(0)))
//   val req1 = Reg(new tiel.HFPIinput)
//   val req2 = Pipe(Reg(next=io.req.valid), io.req, 2)
//   val req4 = Pipe(Reg(next=io.req.valid), io.req, hfvu_latency/2)
//   val req8 = Pipe(Reg(next=io.req.valid), io.req, hfvu_latency)
//   val req_one = Reg(new tiel.HFPIinput)
//   req1 := io.req
//   
//   // sum/ave
//   val hfma0 = Module(new tile.HFPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
//   hfma0.io.in.valid := io.req.valid
//   hfma0.io.in.bits := io.req.bits
//   hfma0.io.in.bits.in1 := in0
//   hfma0.io.in.bits.in2 := in1
//
//   val hfma1 = Module(new tile.HFPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
//   hfma1.io.in.valid := io.req.valid
//   hfma1.io.in.bits := io.req.bits
//   hfma1.io.in.bits.in1 := in2
//   hfma1.io.in.bits.in2 := in3
//
//   val hfma0_out = hfma0.io.out.bits.data
//   val hfma1_out = hfma1.io.out.bits.data
//   val hfma_s1_valid = hfma0.io.out.valid && hfma1.io.out.valid
//   val hfma_s1_out0  = Mux(hfma0_out(15,14) === UInt(3) && hfma0_out(9,0) === UInt(0), Cat(hfma0_out(16),maxN),
//   val hfma_s1_out1  = Mux(hfma1_out(15,14) === UInt(3) && hfma1_out(9,0) === UInt(0), Cat(hfma1_out(16),maxN),
//   val hfma_s1_exc   = Pipe(Reg(next=req4valid),hfma0.io.out.bits.exc | hfma1.io.out.bits.exc, hfvu_latency) 
//
//   val hfma2 = Module(new tile.HFPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
//   hfma2.io.in.valid := hfma_s1_valid && req.valid
//   hfma2.io.in.bits := req4.bits
//   hfma2.io.in.bits.in1 := Cat(Fill(51,UInt(0),hfma_s1_out0))
//   hfma2.io.in.bits.in2 := Cat(Fill(51,UInt(0),hfma_s1_out1))
//
//   val ave = req8.bits.cmd.toint
//   val hfma2_out = hfma2.io.out.bits.data
//   val hfma2_out_fN = hardfloat.fNFromRecFN(5, 11, hfma2_out(16,0))
//   val hfma2_exp = Mux(hfma2_out_fN(14,10) > UInt(1), UInt(hfma2_out_fN(15,11)) - UInt(2), UInt(1,5))
//   val hfma2_fN = Cat(hfma2_out_fN(15), hfma2_exp, hfma2_out_fN(9,0))
//   val hfma2_ave = hardfloat.recFNFromFN(5, 11, hfma2_ed)
//   val hfma2_res = Mux(ave, Cat(Fill(51,UInt(0)),hfma2_ave), hfma2_out)
//   val hfma_s2_valid := hfma2.io.out.valid
//   val hfma_s2_out  := Cat(Fill(51,UInt(0)),
//                           Mux(hfma2_res(15,14) === UInt(3) && hfma2_res(9,0) === UInt(0), Cat(hfma2_res(16),maxN))
//   val hfma_s2_exc  := hfma2.io.out.bits.exc | hfma_s1_exc 
//
//
//   // min/max
//   val hcmp0 = Module(new hardfloat.CompareRecFN(5,11))
//   hcmp0.io.a := 
// 
//   val hfpiu0 = Module(new tile.HFPToInt)
//   hfpiu0.io.in.valid := io.req.valid
//   hfpiu0.io.in.bits := io.req.bits
//   hfpiu0.io.in.bits.in1 := in0
//   hfpiu0.io.in.bits.in2 := in1
//
//   val hfpiu1 = Module(new tile.HFPToInt)
//   hfpiu1.io.in.valid := io.req.valid
//   hfpiu1.io.in.bits := io.req.bits
//   hfpiu1.io.in.bits.in1 := in2
//   hfpiu1.io.in.bits.in2 := in3
//
//   val hfpiu_s2_in0 = cat(Fill(51,UInt(0),req1.bits.in1(16,0)))
//   val hfpiu_s2_in1 = cat(Fill(51,UInt(0),req1.bits.in1(33,17)))
//   val hfpiu_s2_in2 = cat(Fill(51,UInt(0),req1.bits.in1(50,34)))
//   val hfpiu_s2_in3 = cat(Fill(51,UInt(0),req1.bits.in1(67,51)))
//   val max1 = req1.bits.cmd.tohfp
//   val hfpiu0_out = hfpiu0.io.out.bits.toint
//   val hfpiu1_out = hfpiu1.io.out.bits.toint
//   val hfpiu_s1_valid = hfpiu0.io.out.valid && hfpiu1.io.out.valid
//   //??val hfpiu_s1_out0 = Mux(max1 && hfpiu0_out(0))
//   val hfpiu2 = Module(new tile.HFPToInt)
//   hfpiu2.io.in.valid := io.req.valid
//   hfpiu2.io.in.bits := io.req.bits
//   hfpiu2.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(50)),io.req.bits.in1(50,34))
//   hfpiu2.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(50)),io.req.bits.in2(50,34))
//
//
//   val max2 = req2.bits.cmd.tohfp
//   val hfpmu2 = Module(new tile.HFPToHFP(hfpu_latency))
//   hfpmu2.io.in.valid := io.req.valid
//   hfpmu2.io.in.bits := io.req.bits
//   hfpmu2.io.lt := io.lt(2)
//   hfpmu2.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(50)),io.req.bits.in1(50,34))
//   hfpmu2.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(50)),io.req.bits.in2(50,34))
//
//   io.res.bits.exc := hfma0.io.out.bits.exc | hfma1.io.out.bits.exc | hfma2.io.out.bits.exc | hfma3.io.out.bits.exc
//   io.res.bits.data := Cat(Mux(out3(15,14) === UInt(3) && out3(9,0) === UInt(0), Cat(out3(16),maxN),
//                               Mux(out3(15,13) === UInt(0), Cat(out3(16), zero), out3(16,0))),
//                           Mux(out2(15,14) === UInt(3) && out2(9,0) === UInt(0), Cat(out2(16),maxN),
//                               Mux(out2(15,13) === UInt(0), Cat(out3(16), zero), out2(16,0))),
//                           Mux(out1(15,14) === UInt(3) && out1(9,0) === UInt(0), Cat(out1(16),maxN),
//                               Mux(out1(15,13) === UInt(0), Cat(out1(16), zero), out1(16,0))),
//                           Mux(out0(15,14) === UInt(3) && out0(9,0) === UInt(0), Cat(out0(16),maxN),
//                               Mux(out0(15,13) === UInt(0), Cat(out0(16), zero), out0(16,0))))
//}

}
