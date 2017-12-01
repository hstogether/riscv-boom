//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

// Jecy 20171130
// FP16 unit

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

   val default: List[BitPat] = List(FCMD_X,    X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X)

   val table: Array[(BitPat, List[BitPat])] =
      // Note: not all of these signals are used or necessary, but we're
      // constrained by the need to fit the rocket.FPU units' ctrl signals.
      //                                                  swap12         div
      //                                                  | swap32       | sqrt
      //                          cmd                     | | single     | | round
      //                          |            ldst       | | | fromint  | | | wflags
      //                          |            | wen      | | | | toint  | | | |
      //                          |            | | ren1   | | | | | fastpipe | |
      //                          |            | | | ren2 | | | | | | fma| | | |
      //                          |            | | | | ren3 | | | | | |  | | | |
      //                          |            | | | | |  | | | | | | |  | | | |
      Array(
      BitPat(uopFCLASS_H) -> List(FCMD_MV_XF,  X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,N),
      BitPat(uopFMV_H_X)  -> List(FCMD_MV_FX,  X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,N),
      BitPat(uopFMV_X_H)  -> List(FCMD_MV_XF,  X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,N),
      BitPat(uopFCVT_S_H) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,Y,N,N,Y,N, N,N,Y,Y),
      BitPat(uopFCVT_H_S) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,Y,Y),
      BitPat(uopFCVT_S_D) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,Y,Y),
      BitPat(uopFCVT_D_S) -> List(FCMD_CVT_FF, X,X,Y,N,N, N,X,N,N,N,Y,N, N,N,Y,Y),

      BitPat(uopFCVT_H_W) -> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_H_WU)-> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_H_L) -> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,Y),
      BitPat(uopFCVT_H_LU)-> List(FCMD_CVT_FI, X,X,N,N,N, X,X,N,Y,N,N,N, N,N,Y,Y),

      BitPat(uopFCVT_W_H) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_WU_H)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_L_H) -> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,Y),
      BitPat(uopFCVT_LU_H)-> List(FCMD_CVT_IF, X,X,Y,N,N, N,X,N,N,Y,N,N, N,N,Y,Y),

      BitPat(uopFEQ_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,N,Y,N,N, N,N,N,Y),
      BitPat(uopFLT_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,N,Y,N,N, N,N,N,Y),
      BitPat(uopFLE_H)    -> List(FCMD_CMP,    X,X,Y,Y,N, N,N,N,N,Y,N,N, N,N,N,Y),

      BitPat(uopFSGNJ_H)  -> List(FCMD_SGNJ,   X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,N),

      BitPat(uopFMIN_H)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,Y),
      BitPat(uopFMAX_H)   -> List(FCMD_MINMAX, X,X,Y,Y,N, N,N,N,N,N,Y,N, N,N,N,Y),

      BitPat(uopFADD_H)   -> List(FCMD_ADD,    X,X,Y,Y,N, N,Y,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFSUB_H)   -> List(FCMD_SUB,    X,X,Y,Y,N, N,Y,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMUL_H)   -> List(FCMD_MUL,    X,X,Y,Y,N, N,N,N,N,N,N,Y, N,N,Y,Y),

      BitPat(uopFMADD_H)  -> List(FCMD_MADD,   X,X,Y,Y,Y, N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFMSUB_H)  -> List(FCMD_MSUB,   X,X,Y,Y,Y, N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMADD_H) -> List(FCMD_NMADD,  X,X,Y,Y,Y, N,N,N,N,N,N,Y, N,N,Y,Y),
      BitPat(uopFNMSUB_H) -> List(FCMD_NMSUB,  X,X,Y,Y,Y, N,N,N,N,N,N,Y, N,N,Y,Y)
      )

   val decoder = rocket.DecodeLogic(io.uopc, default, table)

   val s = io.sigs
   val sigs = Seq(s.cmd, s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                  s.swap23, s.single, s.fromint, s.toint, s.fastpipe, s.fma,
                  s.div, s.sqrt, s.round, s.wflags)
   sigs zip decoder map {case(s,d) => s := d}
}


class HFpuReq()(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop      = new MicroOp()
   val rs1_data = Bits(width = 65)
   val rs2_data = Bits(width = 65)
   val rs3_data = Bits(width = 65)
   val fcsr_rm  = Bits(width = tile.FPConstants.RM_SZ)
}

class HFPU(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val req = new ValidIO(new FpuReq).flip
      val resp = new ValidIO(new ExeUnitResp(65))
   })

   // all FP units are padded out to the same latency for easy scheduling of the write port
   val hfpu_latency = hfmaLatency
   val io_req = io.req.bits

   val hfp_decoder = Module(new UOPCodeHFPUDecoder)
   hfp_decoder.io.uopc:= io_req.uop.uopc
   val hfp_ctrl = hfp_decoder.io.sigs
   val hfp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === Bits(7), io_req.fcsr_rm, ImmGenRm(io_req.uop.imm_packed))

   val req = Wire(new tile.FPInput)
   req := hfp_ctrl
   req.rm := hfp_rm
   req.in1 := io_req.rs1_data
   req.in2 := io_req.rs2_data
   req.in3 := io_req.rs3_data
   when (hfp_ctrl.swap23) { req.in3 := io_req.rs2_data }

   req.typ := ImmGenTyp(io_req.uop.imm_packed)


   val hfma = Module(new tile.FPUFMAPipe(latency = hfpu_latency, expWidth = 5, sigWidth = 11))
   hfma.io.in.valid := io.req.valid && hfp_ctrl.fma && !hfp_ctrl.single
   hfma.io.in.bits := req


   val fpiu = Module(new tile.FPToInt)
   fpiu.io.in.valid := io.req.valid && (hfp_ctrl.toint || hfp_ctrl.cmd === FCMD_MINMAX)
   fpiu.io.in.bits := req
   val fpiu_out = Pipe(Reg(next=fpiu.io.in.valid && !hfp_ctrl.fastpipe),
                       fpiu.io.out.bits, hfpu_latency-1)
   val fpiu_result  = Wire(new tile.FPResult)
   fpiu_result.data := fpiu_out.bits.toint
   fpiu_result.exc  := fpiu_out.bits.exc


   val fpmu = Module(new tile.FPToFP(hfpu_latency)) // latency 2 for rocket
   fpmu.io.in.valid := io.req.valid && hfp_ctrl.fastpipe
   fpmu.io.in.bits := req
   fpmu.io.lt := fpiu.io.out.bits.lt

   // Response (all FP units have been padded out to the same latency)
   io.resp.valid := fpiu_out.valid ||
                    fpmu.io.out.valid ||
                    hfma.io.out.valid
   val hfpu_out   = Mux(hfma.io.out.valid, hfma.io.out.bits,
                   Mux(fpiu_out.valid,    fpiu_result,
                                          fpmu.io.out.bits))

   io.resp.bits.data              := hfpu_out.data
   io.resp.bits.fflags.valid      := io.resp.valid
   io.resp.bits.fflags.bits.flags := hfpu_out.exc

// TODO why is this assertion failing?
//   assert (PopCount(Vec(ifpu.io.out, fpiu_out, fpmu.io.out, sfma.io.out, dfma.io.out).map(_.valid)) <= UInt(1),
//      "Multiple FPU units are firing requests.")
}

}
