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

      BitPat(uopFCVT_W_H) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,Y,N,Y,N,N,N,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_WU_H)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,Y,N,Y,N,N,N,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_L_H) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,Y,N,Y,N,N,N,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_LU_H)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,Y,N,Y,N,N,N,N,N,N, N,N,Y,Y),

      BitPat(uopFEQ_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,Y,N,Y,N,N,N,N,N,N, N,N,N,Y),
      BitPat(uopFLT_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,Y,N,Y,N,N,N,N,N,N, N,N,N,Y),
      BitPat(uopFLE_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,Y,N,Y,N,N,N,N,N,N, N,N,N,Y),

      BitPat(uopFSGNJ_H)  -> List(FCMD_SGNJ,   X,X,Y,Y,N, N,N,N,Y,N,N,N,N,N,N,Y,N, N,N,N,N),

      BitPat(uopFMIN_H)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,Y,N,N,N,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFMAX_H)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,Y,N,N,N,N,N,N,Y,N, N,N,N,Y),

      BitPat(uopFADD_H)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFSUB_H)   -> List(FCMD_SUB,    X,X,Y,Y,N, N,Y,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMUL_H)   -> List(FCMD_MUL,    X,X,Y,Y,N, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),

      BitPat(uopFMADD_H)  -> List(FCMD_MADD,   X,X,Y,Y,Y, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMSUB_H)  -> List(FCMD_MSUB,   X,X,Y,Y,Y, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMADD_H) -> List(FCMD_NMADD,  X,X,Y,Y,Y, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMSUB_H) -> List(FCMD_NMSUB,  X,X,Y,Y,Y, N,N,N,Y,N,N,N,N,N,N,N,Y, N,N,Y,Y)
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
   if(DEBUG_PRINTF_HFPU_PATH){
      printf("==========[Come into HFPU]==========\n")
   }

   val io = IO(new Bundle
   {
      val req = new ValidIO(new FpuReq).flip
      val resp = new ValidIO(new ExeUnitResp(65))
   })

   // all FP units are padded out to the same latency for easy scheduling of the write port
   val hfpu_latency = hfmaLatency
   val io_req = io.req.bits

   val hfp_decoder = Module(new UOPCodeHFPUDecoder)
   hfp_decoder.io.uopc:= io_req.uop.uopc // 微操作码，eg：uopADD_H
   val hfp_ctrl = hfp_decoder.io.sigs
   val hfp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === Bits(7), io_req.fcsr_rm, ImmGenRm(io_req.uop.imm_packed)) // 选择舍入方式(111寄存器，其他指令中)

   val req = Wire(new tile.FPInput)
   req := hfp_ctrl
   req.rm := hfp_rm
   req.in1 := io_req.rs1_data
   req.in2 := io_req.rs2_data
   req.in3 := io_req.rs3_data
   when (hfp_ctrl.swap23) { req.in3 := io_req.rs2_data }

   req.typ := ImmGenTyp(io_req.uop.imm_packed) // 获得当前指令的类型，00单精度；01双精度；10半精度


   val hfma = Module(new tile.FPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
   hfma.io.in.valid := io.req.valid && hfp_ctrl.fma && hfp_ctrl.half && !hfp_ctrl.single // TODO:FCVT ??  req.typ === Bits(2)
   hfma.io.in.bits := req


   val hfpiu = Module(new tile.HFPToInt)
   hfpiu.io.in.valid := io.req.valid && (hfp_ctrl.toint || hfp_ctrl.cmd === FCMD_MINMAX)
   hfpiu.io.in.bits := req
   val hfpiu_out = Pipe(Reg(next=hfpiu.io.in.valid && !hfp_ctrl.fastpipe),
                       hfpiu.io.out.bits, hfpu_latency-1)
   val hfpiu_result  = Wire(new tile.FPResult)
   hfpiu_result.data := hfpiu_out.bits.toint
   hfpiu_result.exc  := hfpiu_out.bits.exc
 
   val hfpfu = Module(new tile.HFPToFP)
   hfpfu.io.in.valid := io.req.valid && hfp_ctrl.tofp
   hfpfu.io.in.bits :=req
   val hfpfu_out = Pipe(Reg(next=hfpfu.io.in.valid && !hfp_ctrl.fastpipe),
                        hfpfu.io.out.bits, hfpu_latency-1)
   val hfpfu_result = Wire(new tile.FPResult)
   hfpfu_result.data := hfpfu_out.bits.data
   hfpfu_result.exc  := hfpfu_out.bits.exc

   // Modify to work for HFP
   val hfpmu = Module(new tile.HFPToHFP(hfpu_latency)) // latency 2 for rocket
   hfpmu.io.in.valid := io.req.valid && hfp_ctrl.fastpipe
   hfpmu.io.in.bits := req
   hfpmu.io.lt := hfpiu.io.out.bits.lt

   // Response (all HFP units have been padded out to the same latency)
   io.resp.valid := hfpiu_out.valid ||
                    hfpmu.io.out.valid ||
                    hfma.io.out.valid
   val hfpu_out   = Mux(hfma.io.out.valid, hfma.io.out.bits,
                    Mux(hfpiu_out.valid,   hfpiu_result,
                    Mux(hfpfu_out.valid,   hfpfu_result,
                                           hfpmu.io.out.bits)))

   io.resp.bits.data              := hfpu_out.data
   io.resp.bits.fflags.valid      := io.resp.valid
   io.resp.bits.fflags.bits.flags := hfpu_out.exc

   if(DEBUG_PRINTF_HFPU){
      printf("HFPU-Start--------------------------------------------------------------------------------------------\n")
      printf("io.req.bits.uop.uopc=[%d]    req.cmd=[%d]    req.rs1=[%x]    req.rs2=[%x]    req.rs3=[%x]\n",
              io_req.uop.uopc,             req.cmd,        io_req.rs1_data,io_req.rs2_data,io_req.rs3_data);
      //printf("  hfma.io.in.valid := io.req.valid && hfp_ctrl.fma && hfp_ctrl.half && !hfp_ctrl.single\n")
      //printf("  %d                  %d              %d              %d               %d\n",
      //          hfma.io.in.valid.asUInt,io.req.valid.asUInt,hfp_ctrl.fma.asUInt,hfp_ctrl.half.asUInt,(!hfp_ctrl.single).asUInt)
      //printf("  io_req.uop.imm_packed=[%d]    ImmGenTyp(io_req.uop.imm_packed)=[%d]    req.typ=[%d]\n",
      //          io_req.uop.imm_packed,        ImmGenTyp(io_req.uop.imm_packed),        req.typ)
      //printf("hfma.io.in.valid=[%d]    hfma.io.rs1=[%x]    hfma.io.rs2=[%x]    hfma.io.rs3=[%x]\n",
      //        hfma.io.in.valid.asUInt, hfma.io.in.bits.in1,hfma.io.in.bits.in2,hfma.io.in.bits.in3);
      printf("hfpfu.io.in.valid=[%d]    !hfp_ctrl.fastpipe=[%d]    hfpfu.io.out.bits.data=[%x]    hfpu_latency-1=[%d]\n",
              hfpfu.io.in.valid.asUInt, (!hfp_ctrl.fastpipe).asUInt,hfpfu.io.out.bits.data,       UInt(hfpu_latency-1))
      printf("hfma.io.in.valid=[%d]    hfpiu.io.in.valid=[%d]    hfpfu.io.in.valid=[%d]    hfpmu.io.in.valid=[%d]\n",
              hfma.io.in.valid.asUInt, hfpiu.io.in.valid.asUInt, hfpfu.io.in.valid.asUInt, hfpmu.io.in.valid.asUInt)
      printf("hfma.io.out.valid=[%d]  hfpiu_out.valid =[%d]     hfpfu_out.valid=[%d]      hfpmu.io.out.valid=[%d]\n",
              hfma.io.out.valid.asUInt, hfpiu_out.valid.asUInt, hfpfu_out.valid.asUInt,   hfpmu.io.out.valid.asUInt)
      printf("hfma.io.out=[%x]         hfpiu_result=[%x]    hfpfu_result=[%x]    hfpmu.io.out=[%x]\n",
              hfma.io.out.bits.data,   hfpiu_result.data,   hfpfu_result.data,   hfpmu.io.out.bits.data);
      printf("resp.data=[%x]    resp.valid=[%d]\n",io.resp.bits.data,io.resp.valid.asUInt);
      printf("HFPU-End--------------------------------------------------------------------------------------------\n")
   }


// TODO why is this assertion failing?
//   assert (PopCount(Vec(ifpu.io.out, fpiu_out, fpmu.io.out, sfma.io.out, dfma.io.out).map(_.valid)) <= UInt(1),
//      "Multiple FPU units are firing requests.")
}


}
