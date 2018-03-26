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
class UOPCodeHFPUDecoder extends Module
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
      BitPat(uopFCLASS_H) -> List(FCMD_MV_XF,  X,X,Y,N,N, N,X,N,Y,N,Y,N,N,N,N,N,N, N,N,Y,N),
      BitPat(uopFMV_H_X)  -> List(FCMD_MV_FX,  X,X,N,N,N, X,X,N,Y,Y,N,N,N,N,Y,N,N, N,N,Y,N),
      BitPat(uopFMV_X_H)  -> List(FCMD_MV_XF,  X,X,Y,N,N, N,X,N,Y,N,Y,N,N,Y,N,N,N, N,N,Y,N),
      BitPat(uopFMV_H_F)  -> List(FCMD_MV_HF,  X,X,Y,N,N, N,X,N,Y,N,N,Y,N,N,Y,N,N, N,N,Y,N),
      BitPat(uopFMV_F_H)  -> List(FCMD_MV_FH,  X,X,Y,N,N, N,X,N,Y,N,N,N,Y,Y,N,N,N, N,N,Y,N),
      BitPat(uopFCVT_H_S) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,Y,Y,N,N,Y,N,N,Y,N,N, N,N,Y,Y), // TODO: is Single? -- Jecy is fastpipe? Y
      BitPat(uopFCVT_H_D) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,N,Y,N,N,Y,N,N,Y,N,N, N,N,Y,Y), // if fastpipe? Y
      BitPat(uopFCVT_S_H) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,Y,Y,N,N,N,Y,Y,N,N,N, N,N,Y,Y), // TODO: is Single? -- Jecy is fastpipe? Y
      BitPat(uopFCVT_D_H) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,N,Y,N,N,N,Y,Y,N,N,N, N,N,Y,Y),

      BitPat(uopFCVT_H_W) -> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,Y,N,N,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_H_WU)-> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,Y,N,N,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_H_L) -> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,Y,N,N,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_H_LU)-> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,Y,N,N,N,N,Y,N,N, N,N,Y,Y),

      BitPat(uopFCVT_W_H) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,Y,N,Y,N,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_WU_H)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,Y,N,Y,N,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_L_H) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,Y,N,Y,N,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_LU_H)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,Y,N,Y,N,N,Y,N,N,N, N,N,Y,Y),

      BitPat(uopFEQ_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,Y,N,Y,N,N,N,N,N,N, N,N,N,Y),
      BitPat(uopFLT_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,Y,N,Y,N,N,N,N,N,N, N,N,N,Y),
      BitPat(uopFLE_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,Y,N,Y,N,N,N,N,N,N, N,N,N,Y),

      BitPat(uopFSGNJ_H)  -> List(FCMD_SGNJ,   X,X,Y,Y,N, N,N,N,Y,N,N,N,N,N,N,Y,N, N,N,N,N),

      BitPat(uopFMIN_H)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,Y,N,N,N,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFMAX_H)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,Y,N,N,N,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopRELU_H)   -> List(FCMD_MINMAX, X,X,Y,N,N, N,N,N,Y,N,N,N,N,N,N,Y,N, N,N,N,Y),

      BitPat(uopFADD_H)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFSUB_H)   -> List(FCMD_SUB,    X,X,Y,Y,N, N,Y,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMUL_H)   -> List(FCMD_MUL,    X,X,Y,Y,N, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),

      BitPat(uopFMADD_H)  -> List(FCMD_MADD,   X,X,Y,Y,Y, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMSUB_H)  -> List(FCMD_MSUB,   X,X,Y,Y,Y, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMADD_H) -> List(FCMD_NMADD,  X,X,Y,Y,Y, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMSUB_H) -> List(FCMD_NMSUB,  X,X,Y,Y,Y, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),

      BitPat(uopLSA0_H)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopLSA1_H)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopLSA2_H)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopLSA3_H)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y)

      )

   val decoder = rocket.DecodeLogic(io.uopc, default, table)

   val s = io.sigs
   val sigs = Seq(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.single, s.half, s.fromint, s.toint, s.fromfp, s.tofp,
                  s.fromhfp, s.tohfp, s.fastpipe, s.fma, s.div, s.sqrt, s.round,
                  s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}

class HFPU(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val req = new ValidIO(new FpuReq).flip
      val resp = new ValidIO(new ExeUnitResp(68))
   })

   // all FP units are padded out to the same latency for easy scheduling of the write port
   val hfpu_latency = hfmaLatency
   val io_req = io.req.bits

   val hfp_decoder = Module(new UOPCodeHFPUDecoder)
   hfp_decoder.io.uopc:= io_req.uop.uopc // 微操作码，eg：uopADD_H
   val hfp_ctrl = hfp_decoder.io.sigs
   val hfp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === Bits(7), io_req.fcsr_rm, ImmGenRm(io_req.uop.imm_packed)) // 选择舍入方式(111寄存器，其他指令中)

   val req = Wire(new tile.HFPInput)
   req := hfp_ctrl
   req.rm := hfp_rm
   req.in2 := io_req.rs2_data
   req.in3 := io_req.rs3_data
   req.in1 := Mux(io_req.uop.uopc === uopLSA1_H, Cat(io_req.rs1_data(50,0), Fill(17,UInt(0))),
              Mux(io_req.uop.uopc === uopLSA2_H, Cat(io_req.rs1_data(33,0), Fill(34,UInt(0))),
              Mux(io_req.uop.uopc === uopLSA3_H, Cat(io_req.rs1_data(16,0), Fill(51,UInt(0))),
                  io_req.rs1_data)))
   when (hfp_ctrl.cmd === FCMD_MINMAX && !hfp_ctrl.ren2 ) { req.in2 := UInt(0) }
   when (hfp_ctrl.swap23) { req.in3 := io_req.rs2_data }

   req.typ := ImmGenTyp(io_req.uop.imm_packed) // 获得当前指令的类型，00单精度；01双精度；10半精度


   //val hfma = Module(new tile.HFPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
   //hfma.io.in.valid := io.req.valid && hfp_ctrl.fma && hfp_ctrl.half && !hfp_ctrl.single
   //hfma.io.in.bits := req
   val hfma = Module(new HFMA())
   hfma.io.req.valid := io.req.valid && hfp_ctrl.fma && hfp_ctrl.half && !hfp_ctrl.single
   hfma.io.req.bits := req


   //val hfpiu = Module(new tile.HFPToInt)
   //hfpiu.io.in.valid := io.req.valid && (hfp_ctrl.toint || hfp_ctrl.cmd === FCMD_MINMAX)
   //hfpiu.io.in.bits := req
   //val hfpiu_out = Pipe(Reg(next=hfpiu.io.in.valid && !hfp_ctrl.fastpipe),
   //                    hfpiu.io.out.bits, hfpu_latency-1)
   //val hfpiu_result  = Wire(new tile.HFPResult)
   //hfpiu_result.data := hfpiu_out.bits.toint
   //hfpiu_result.exc  := hfpiu_out.bits.exc
   val hfpiu = Module(new HFPIU())
   hfpiu.io.req.valid := io.req.valid && (hfp_ctrl.toint || hfp_ctrl.cmd === FCMD_MINMAX)
   hfpiu.io.req.bits  := req
 
   val hfpfu = Module(new tile.HFPToFP)
   hfpfu.io.in.valid := io.req.valid && hfp_ctrl.tofp
   hfpfu.io.in.bits :=req
   val hfpfu_out = Pipe(Reg(next=hfpfu.io.in.valid && !hfp_ctrl.fastpipe),
                        hfpfu.io.out.bits, hfpu_latency-1)
   val hfpfu_result = Wire(new tile.HFPResult)
   hfpfu_result.data := hfpfu_out.bits.data
   hfpfu_result.exc  := hfpfu_out.bits.exc

   //val hfpmu = Module(new tile.HFPToHFP(hfpu_latency)) // latency 2 for rocket
   //hfpmu.io.in.valid := io.req.valid && hfp_ctrl.fastpipe
   //hfpmu.io.in.bits := req
   //hfpmu.io.lt := hfpiu.io.out.bits.lt
   //hfpmu.io.lt := hfpiu.io.lt

   val hfpmu = Module(new HFPMU())
   hfpmu.io.req.valid := io.req.valid && hfp_ctrl.fastpipe
   hfpmu.io.req.bits  := req
   hfpmu.io.lt        := hfpiu.io.lt

   // Response (all HFP units have been padded out to the same latency)
   io.resp.valid := hfpiu.io.res.valid ||
                    hfpmu.io.res.valid ||
                    hfma.io.res.valid ||
                    hfpfu_out.valid
   val hfpu_out   = Mux(hfma.io.res.valid,  hfma.io.res.bits,
                    Mux(hfpiu.io.res.valid, hfpiu.io.res.bits,
                    Mux(hfpfu_out.valid,    hfpfu_result,
                                            hfpmu.io.res.bits)))

   //io.resp.valid := hfpiu_out.valid ||
   //                 hfpmu.io.out.valid ||
   //                 hfma.io.out.valid
   //val hfpu_out   = Mux(hfma.io.res.valid, hfma.io.out.bits,
   //                 Mux(hfpiu_out.valid,   hfpiu_result,
   //                 Mux(hfpfu_out.valid,   hfpfu_result,
   //                                        hfpmu.io.out.bits)))


   io.resp.bits.data              := hfpu_out.data
   io.resp.bits.fflags.valid      := io.resp.valid
   io.resp.bits.fflags.bits.flags := hfpu_out.exc

   if(DEBUG_PRINTF_HFPU){
      printf("HFPU-Start--------------------------------------------------------------------------------------------\n")
      printf("io.req.bits.uop.uopc=[%d]    req.cmd=[%d]    req.rm=[%d]    req.rs1=[%x]    req.rs2=[%x]    req.rs3=[%x]\n",
              io_req.uop.uopc,             req.cmd,        req.rm,        io_req.rs1_data,io_req.rs2_data,io_req.rs3_data);
      //printf("  hfma.io.in.valid := io.req.valid && hfp_ctrl.fma && hfp_ctrl.half && !hfp_ctrl.single\n")
      //printf("  %d                  %d              %d              %d               %d\n",
      //          hfma.io.in.valid.asUInt,io.req.valid.asUInt,hfp_ctrl.fma.asUInt,hfp_ctrl.half.asUInt,(!hfp_ctrl.single).asUInt)
      //printf("  io_req.uop.imm_packed=[%d]    ImmGenTyp(io_req.uop.imm_packed)=[%d]    req.typ=[%d]\n",
      //          io_req.uop.imm_packed,        ImmGenTyp(io_req.uop.imm_packed),        req.typ)
      //printf("hfma.io.in.valid=[%d]    hfma.io.rs1=[%x]    hfma.io.rs2=[%x]    hfma.io.rs3=[%x]\n",
      //        hfma.io.in.valid.asUInt, hfma.io.in.bits.in1,hfma.io.in.bits.in2,hfma.io.in.bits.in3);
      printf("hfpfu.io.in.valid=[%d]    !hfp_ctrl.fastpipe=[%d]    hfpfu.io.out.bits.data=[%x]    hfpu_latency-1=[%d]\n",
              hfpfu.io.in.valid.asUInt, (!hfp_ctrl.fastpipe).asUInt,hfpfu.io.out.bits.data,       UInt(hfpu_latency-1))
      printf("hfma.io.req.valid=[%d]    hfpiu.io.in.valid=[%d]    hfpfu.io.in.valid=[%d]    hfpmu.io.req.valid=[%d]\n",
              hfma.io.req.valid.asUInt, hfpiu.io.req.valid.asUInt, hfpfu.io.in.valid.asUInt, hfpmu.io.req.valid.asUInt)
      printf("hfma.io.res.valid=[%d]  hfpiu_out.valid =[%d]     hfpfu_out.valid=[%d]      hfpmu.io.res.valid=[%d]\n",
              hfma.io.res.valid.asUInt, hfpiu.io.res.valid.asUInt, hfpfu_out.valid.asUInt,   hfpmu.io.res.valid.asUInt)
      printf("hfma.io.res=[%x][%d]    hfpiuio.res=[%x][%d]    hfpfu_result=[%x][%d]    hfpmu.io.out=[%x][%d]\n",
              hfma.io.res.bits.data, hfma.io.res.bits.exc,   
              hfpiu.io.res.bits.data, hfpiu.io.res.bits.exc,
              hfpfu_result.data, hfpfu_result.exc,
              hfpmu.io.res.bits.data, hfpmu.io.res.bits.exc);
      printf("resp.data=[%x][%d]    resp.valid=[%d]\n",
              io.resp.bits.data, io.resp.bits.fflags.bits.flags, io.resp.valid.asUInt);
      printf("HFPU-End--------------------------------------------------------------------------------------------\n")
   }


// TODO why is this assertion failing?
//   assert (PopCount(Vec(ifpu.io.out, fpiu_out, fpmu.io.out, sfma.io.out, dfma.io.out).map(_.valid)) <= UInt(1),
//      "Multiple FPU units are firing requests.")
}

class HFMA(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle {
      val req = Valid(new tile.HFPInput).flip
      val res = Valid(new tile.HFPResult)
   }
   val hfpu_latency = hfmaLatency

   val hfma0 = Module(new tile.HFPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
   hfma0.io.in.valid := io.req.valid
   hfma0.io.in.bits := io.req.bits

   val hfma1 = Module(new tile.HFPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
   hfma1.io.in.valid := io.req.valid
   hfma1.io.in.bits := io.req.bits
   hfma1.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(33)),io.req.bits.in1(33,17))
   hfma1.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(33)),io.req.bits.in2(33,17))
   hfma1.io.in.bits.in3 := Cat(Fill(51,io.req.bits.in3(33)),io.req.bits.in3(33,17))

   val hfma2 = Module(new tile.HFPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
   hfma2.io.in.valid := io.req.valid
   hfma2.io.in.bits := io.req.bits
   hfma2.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(50)),io.req.bits.in1(50,34))
   hfma2.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(50)),io.req.bits.in2(50,34))
   hfma2.io.in.bits.in3 := Cat(Fill(51,io.req.bits.in3(50)),io.req.bits.in3(50,34))

   val hfma3 = Module(new tile.HFPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
   hfma3.io.in.valid := io.req.valid
   hfma3.io.in.bits := io.req.bits
   hfma3.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(67)),io.req.bits.in1(67,51))
   hfma3.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(67)),io.req.bits.in2(67,51))
   hfma3.io.in.bits.in3 := Cat(Fill(51,io.req.bits.in3(67)),io.req.bits.in3(67,51))

   val out0 = hfma0.io.out.bits.data
   val out1 = hfma1.io.out.bits.data
   val out2 = hfma2.io.out.bits.data
   val out3 = hfma3.io.out.bits.data
   val maxN = Cat(Fill(4,Bits(1)),Bits(0),Fill(10,Bits(1)))
   val zero = Cat(Fill(57,Bits(0)))
   io.res.valid := hfma0.io.out.valid && hfma1.io.out.valid && hfma2.io.out.valid && hfma3.io.out.valid
   io.res.bits.exc := hfma0.io.out.bits.exc | hfma1.io.out.bits.exc | hfma2.io.out.bits.exc | hfma3.io.out.bits.exc
   io.res.bits.data := Cat(Mux(out3(15,14) === UInt(3) && out3(9,0) === UInt(0), Cat(out3(16),maxN),
                               Mux(out3(15,13) === UInt(0), Cat(out3(16), zero), out3(16,0))),
                           Mux(out2(15,14) === UInt(3) && out2(9,0) === UInt(0), Cat(out2(16),maxN),
                               Mux(out2(15,13) === UInt(0), Cat(out3(16), zero), out2(16,0))),
                           Mux(out1(15,14) === UInt(3) && out1(9,0) === UInt(0), Cat(out1(16),maxN),
                               Mux(out1(15,13) === UInt(0), Cat(out1(16), zero), out1(16,0))),
                           Mux(out0(15,14) === UInt(3) && out0(9,0) === UInt(0), Cat(out0(16),maxN),
                               Mux(out0(15,13) === UInt(0), Cat(out0(16), zero), out0(16,0))))
   if(DEBUG_PRINTF_HFPU){
      printf("HFMA---------------------------------------------------------------------------------------------\n")
      printf("HFMA-io.req.valid=[%d]    io.req.bits.in1=[%x]    io.req.bits.in2=[%x]    io.req.bits.in3=[%x]\n\n",
                   io.req.valid.asUInt, io.req.bits.in1,        io.req.bits.in2,        io.req.bits.in3)
      printf("hfma0-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
                   hfma0.io.in.valid.asUInt, hfma0.io.in.bits.in1, hfma0.io.in.bits.in2, hfma0.io.in.bits.in3)  
      printf("hfma1-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
                   hfma1.io.in.valid.asUInt, hfma1.io.in.bits.in1, hfma1.io.in.bits.in2, hfma1.io.in.bits.in3)  
      printf("hfma2-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
                   hfma2.io.in.valid.asUInt, hfma2.io.in.bits.in1, hfma2.io.in.bits.in2, hfma2.io.in.bits.in3)  
      printf("hfma3-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n\n",
                   hfma3.io.in.valid.asUInt, hfma3.io.in.bits.in1, hfma3.io.in.bits.in2, hfma3.io.in.bits.in3)  
      printf("hfma0-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n",
                   hfma0.io.out.valid.asUInt, out0,                    hfma0.io.out.bits.exc)
      printf("hfma1-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n",
                   hfma1.io.out.valid.asUInt, out1,                    hfma1.io.out.bits.exc)
      printf("hfma2-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n",
                   hfma2.io.out.valid.asUInt, out2,                    hfma2.io.out.bits.exc)
      printf("hfma3-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n\n",
                   hfma3.io.out.valid.asUInt, out3,                    hfma3.io.out.bits.exc)
      printf("io.res.valid=[%d]    io.res.bits.exc=[%d]    io.res.bits.data=[%x]    maxN=[%x]\n",
              io.res.valid.asUInt, io.res.bits.exc,        io.res.bits.data,        maxN)
      printf("HFMA---------------------------------------------------------------------------------------------\n")
   }
}

class HFPIU(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle {
      val req = Valid(new tile.HFPInput).flip
      val res = Valid(new tile.HFPResult)
      val lt  = Bits(OUTPUT,4) //Bool(OUTPUT) // TODO: for four
   }
   val hfpu_latency = hfmaLatency
   val req = Reg(new tile.HFPInput)
   req := io.req.bits

   val hfpiu0 = Module(new tile.HFPToInt)
   hfpiu0.io.in.valid := io.req.valid
   hfpiu0.io.in.bits := io.req.bits

   val hfpiu1 = Module(new tile.HFPToInt)
   hfpiu1.io.in.valid := io.req.valid
   hfpiu1.io.in.bits := io.req.bits
   hfpiu1.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(33)),io.req.bits.in1(33,17))
   hfpiu1.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(33)),io.req.bits.in2(33,17))
 
   val hfpiu2 = Module(new tile.HFPToInt)
   hfpiu2.io.in.valid := io.req.valid
   hfpiu2.io.in.bits := io.req.bits
   hfpiu2.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(50)),io.req.bits.in1(50,34))
   hfpiu2.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(50)),io.req.bits.in2(50,34))

   val hfpiu3 = Module(new tile.HFPToInt)
   hfpiu3.io.in.valid := io.req.valid
   hfpiu3.io.in.bits := io.req.bits
   hfpiu3.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(67)),io.req.bits.in1(67,51))
   hfpiu3.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(67)),io.req.bits.in2(67,51))

   val out0 = hfpiu0.io.out.bits
   val out1 = hfpiu1.io.out.bits
   val out2 = hfpiu2.io.out.bits
   val out3 = hfpiu3.io.out.bits
   val out = Wire(new tile.HFPResult)
   out.data := Mux(req.cmd === FCMD_CVT_IF, out0.toint,
               Mux(req.cmd === FCMD_CMP, Cat(Fill(64,Bits(0)),out3.toint(0),out2.toint(0),out1.toint(0),out0.toint(0)),
               Mux(req.rm(0), (out0.toint | out1.toint | out2.toint | out3.toint),
                   Cat(out3.toint(15,0),out2.toint(15,0),out1.toint(15,0),out0.toint(15,0)))))
   out.exc := Mux(req.cmd === FCMD_CVT_IF, out0.exc, (out0.exc | out1.exc | out2.exc | out3.exc))
   val hfpiu_out = Pipe(Reg(next=io.req.valid && !io.req.bits.fastpipe),
                       out, hfpu_latency-1)

   io.res.valid     := hfpiu_out.valid
   io.res.bits.exc  := hfpiu_out.bits.exc
   io.res.bits.data := hfpiu_out.bits.data
   io.lt            := Cat(hfpiu3.io.out.bits.lt, hfpiu2.io.out.bits.lt,
                           hfpiu1.io.out.bits.lt, hfpiu0.io.out.bits.lt)

   if(DEBUG_PRINTF_HFPU){
     printf("HFPIU---------------------------------------------------------------------------------------------\n")
     printf("hfpiu-io.req.valid=[%d]    io.req.bits.in1=[%x]    io.req.bits.in2=[%x]    io.req.bits.in3=[%x]\n\n",
                  io.req.valid.asUInt, io.req.bits.in1,        io.req.bits.in2,        io.req.bits.in3)

     printf("hfpiu0-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
                  hfpiu0.io.in.valid.asUInt, hfpiu0.io.in.bits.in1, hfpiu0.io.in.bits.in2, hfpiu0.io.in.bits.in3)  
     printf("hfpiu1-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
                  hfpiu1.io.in.valid.asUInt, hfpiu1.io.in.bits.in1, hfpiu1.io.in.bits.in2, hfpiu1.io.in.bits.in3)  
     printf("hfpiu2-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
                  hfpiu2.io.in.valid.asUInt, hfpiu2.io.in.bits.in1, hfpiu2.io.in.bits.in2, hfpiu2.io.in.bits.in3)  
     printf("hfpiu3-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n\n",
                  hfpiu3.io.in.valid.asUInt, hfpiu3.io.in.bits.in1, hfpiu3.io.in.bits.in2, hfpiu3.io.in.bits.in3)  

     printf("hfpiu0-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n",
                  hfpiu0.io.out.valid.asUInt, out0.toint,              hfpiu0.io.out.bits.exc)
     printf("hfpiu1-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n",
                  hfpiu1.io.out.valid.asUInt, out1.toint,              hfpiu1.io.out.bits.exc)
     printf("hfpiu2-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n",
                  hfpiu2.io.out.valid.asUInt, out2.toint,              hfpiu2.io.out.bits.exc)
     printf("hfpiu3-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n\n",
                  hfpiu3.io.out.valid.asUInt, out3.toint,              hfpiu3.io.out.bits.exc)

     printf("req.cmd===FCMD_CMP=[%d]    req.rm(0)=[%d]    req.cmd===FCMD_CVT_IF=[%d]    out.data=[%x]    out.exc=[%d]\n",   
            (req.cmd===FCMD_CMP).asUInt, req.rm(0),      (req.cmd===FCMD_CVT_IF).asUInt,out.data,        out.exc)
     printf("io.req.bits.cmd=[%x]    req.cmd=[%x]\n\n",
             io.req.bits.cmd,        req.cmd)

     printf("io.res.valid=[%d]    io.res.bits.exc=[%d]    io.res.bits.data=[%x]\n",
             io.res.valid.asUInt, io.res.bits.exc,        io.res.bits.data)
     printf("HFPIU---------------------------------------------------------------------------------------------\n")
   }
}


class HFPMU(implicit p: Parameters) extends BoomModule()(p)
{
   val io = new Bundle {
      val req = Valid(new tile.HFPInput).flip
      val res = Valid(new tile.HFPResult)
      val lt  = Bits(INPUT,4)
   }
   val hfpu_latency = hfmaLatency

   val hfpmu0 = Module(new tile.HFPToHFP(hfpu_latency))
   hfpmu0.io.in.valid := io.req.valid
   hfpmu0.io.in.bits := io.req.bits
   hfpmu0.io.lt := io.lt(0)

   val hfpmu1 = Module(new tile.HFPToHFP(hfpu_latency))
   hfpmu1.io.in.valid := io.req.valid
   hfpmu1.io.in.bits := io.req.bits
   hfpmu1.io.lt := io.lt(1)
   hfpmu1.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(33)),io.req.bits.in1(33,17))
   hfpmu1.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(33)),io.req.bits.in2(33,17))

   val hfpmu2 = Module(new tile.HFPToHFP(hfpu_latency))
   hfpmu2.io.in.valid := io.req.valid
   hfpmu2.io.in.bits := io.req.bits
   hfpmu2.io.lt := io.lt(2)
   hfpmu2.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(50)),io.req.bits.in1(50,34))
   hfpmu2.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(50)),io.req.bits.in2(50,34))

   val hfpmu3 = Module(new tile.HFPToHFP(hfpu_latency))
   hfpmu3.io.in.valid := io.req.valid
   hfpmu3.io.in.bits := io.req.bits
   hfpmu3.io.lt := io.lt(3)
   hfpmu3.io.in.bits.in1 := Cat(Fill(51,io.req.bits.in1(67)),io.req.bits.in1(67,51))
   hfpmu3.io.in.bits.in2 := Cat(Fill(51,io.req.bits.in2(67)),io.req.bits.in2(67,51))

   val out0 = hfpmu0.io.out.bits.data
   val out1 = hfpmu1.io.out.bits.data
   val out2 = hfpmu2.io.out.bits.data
   val out3 = hfpmu3.io.out.bits.data

   val valid0 = hfpmu0.io.out.valid
   val valid1 = hfpmu1.io.out.valid
   val valid2 = hfpmu2.io.out.valid
   val valid3 = hfpmu3.io.out.valid

   val exc0 = hfpmu0.io.out.bits.exc
   val exc1 = hfpmu1.io.out.bits.exc
   val exc2 = hfpmu2.io.out.bits.exc
   val exc3 = hfpmu3.io.out.bits.exc

   io.res.valid := valid0 && valid1 && valid2 && valid3
   io.res.bits.exc := exc0 | exc1 | exc2 | exc3
   io.res.bits.data := Cat(out3(16,0),out2(16,0),out1(16,0),out0(16,0))

   if(DEBUG_PRINTF_HFPU){
     printf("HFPMU---------------------------------------------------------------------------------------------\n")
     printf("hfpmu-io.req.valid=[%d]    io.req.bits.in1=[%x]    io.req.bits.in2=[%x]    io.req.bits.in3=[%x]\n\n",
                   io.req.valid.asUInt, io.req.bits.in1,        io.req.bits.in2,        io.req.bits.in3)

     printf("hfpmu0-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
             hfpmu0.io.in.valid.asUInt, hfpmu0.io.in.bits.in1,  hfpmu0.io.in.bits.in2,  hfpmu0.io.in.bits.in3)  
     printf("hfpmu1-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
               hfpmu1.io.in.valid.asUInt, hfpmu1.io.in.bits.in1, hfpmu1.io.in.bits.in2, hfpmu1.io.in.bits.in3)  
     printf("hfpmu2-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n",
               hfpmu2.io.in.valid.asUInt, hfpmu2.io.in.bits.in1, hfpmu2.io.in.bits.in2, hfpmu2.io.in.bits.in3)  
     printf("hfpmu3-io.in.valid=[%d]    io.in.bits.in1=[%x]     io.in.bits.in2=[%x]     io.in.bits.in3=[%x]\n\n",
               hfpmu3.io.in.valid.asUInt, hfpmu3.io.in.bits.in1, hfpmu3.io.in.bits.in2, hfpmu3.io.in.bits.in3)  

     printf("hfpmu0-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n", valid0, out0, exc0)
     printf("hfpmu1-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n", valid1, out1, exc1)
     printf("hfpmu2-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n", valid2, out2, exc2)
     printf("hfpmu3-io.out.bits.valid=[%d]    io.out.bits.data=[%x]    io.out.bits.exc=[%d]\n\n",valid3,out3, exc3)

     printf("io.res.valid=[%d]    io.res.bits.exc=[%d]    io.res.bits.data=[%x]\n",
             io.res.valid.asUInt, io.res.bits.exc,        io.res.bits.data)
     printf("HFPIU---------------------------------------------------------------------------------------------\n")
   }

}

}
