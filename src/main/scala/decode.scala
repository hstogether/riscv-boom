//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------

package boom
{

import Chisel._
import config.Parameters

import rocket.Instructions._
import rocket.{CSR,Causes}
import util.uintToBitPat
import FUConstants._
import uncore.constants.MemoryOpConstants._


abstract trait DecodeConstants
{
// scalastyle:off
  val xpr64 = Y // TODO inform this from xLen

  def decode_default: List[BitPat] =
            //                                                                                                         frs3_en                          wakeup_delay
            //     is val inst?                                                                                        |  imm sel                       |                   bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                                                                      |  |   is_load                   |                   |  br/jmp
            //     |  |  is single-prec?                         rs1 regtype                                           |  |   | is_store                |                   |  |  is jal
            //     |  |  |  is hfp inst?                          |              rs2 type                              |  |   | | is_amo                |                   |  |  |  allocate_brtag
            //     |  |  |  |  micro-code        func unit        |              |                  rs3 type           |  |   | | | is_fence            |                   |  |  |  |
            //     |  |  |  |  |                 |                |              |                  |                  |  |   | | | | is_fencei         |                   |  |  |  |
            //     |  |  |  |  |                 |        dst     |              |                  |                  |  |   | | | | |  mem    mem     |                   |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |  |                 |        regtype |              |                  |                  |  |   | | | | |  cmd    msk     |                   |  |  |  |  |  flush on commit
            //     |  |  |  |  |                 |        |       |              |                  |                  |  |   | | | | |  |      |       |                   |  |  |  |  |  |  csr cmd
              List(N, N, X, X, uopX   , IQT_INT, FU_X   ,RT_X,BitPat.dontCare(3),BitPat.dontCare(3),BitPat.dontCare(3),X,IS_X,X,X,X,X,N, M_X,   MSK_X,  BitPat.dontCare(2), X, X, X, X, N, X, CSR.X)

  val table: Array[(BitPat, List[BitPat])]
// scalastyle:on
}

class CtrlSigs extends Bundle
{
   val legal           = Bool()
   val fp_val          = Bool()
   val fp_single       = Bool()
   val hfp_val         = Bool()
   val uopc            = UInt(width = UOPC_SZ)
   val iqtype          = UInt(width = IQT_SZ)
   val fu_code         = UInt(width = FUC_SZ)
   val dst_type        = UInt(width=3)
   val rs1_type        = UInt(width=3)
   val rs2_type        = UInt(width=3)
   val rs3_type        = UInt(width=3)
   val frs3_en         = Bool()
   val imm_sel         = UInt(width = IS_X.getWidth)
   val is_load         = Bool()
   val is_store        = Bool()
   val is_amo          = Bool()
   val is_fence        = Bool()
   val is_fencei       = Bool()
   val mem_cmd         = UInt(width = M_SZ)
   val mem_typ         = UInt(width = rocket.MT_SZ)
   val wakeup_delay    = UInt(width = 2)
   val bypassable      = Bool()
   val br_or_jmp       = Bool()
   val is_jal          = Bool()
   val allocate_brtag  = Bool()
   val inst_unique     = Bool()
   val flush_on_commit = Bool()
   val csr_cmd         = UInt(width = rocket.CSR.SZ)
   val rocc            = Bool()

   def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
      val decoder = rocket.DecodeLogic(inst, XDecode.decode_default, table)
      val sigs =
         Seq(legal, fp_val, fp_single, hfp_val, uopc, iqtype, fu_code, dst_type, rs1_type
         , rs2_type, rs3_type, frs3_en, imm_sel, is_load, is_store, is_amo
         , is_fence, is_fencei, mem_cmd, mem_typ, wakeup_delay, bypassable
         , br_or_jmp, is_jal, allocate_brtag, inst_unique, flush_on_commit, csr_cmd)
      sigs zip decoder map {case(s,d) => s := d}
      rocc := Bool(false)
      this
   }
}


object XDecode extends DecodeConstants
{
// scalastyle:off
            //                                                                             frs3_en                                wakeup_delay
            //     is val inst?                                                            |  imm sel                             |        bypassable (aka, known/fixed latency)
            //     |  is fp inst?                                                          |  |     is_load                       |        |  br/jmp
            //     |  |  is single-prec?                           rs1 regtype             |  |     |  is_store                   |        |  |  is jal
            //     |  |  |  if hfp inst?                           |       rs2 type        |  |     |  |  is_amo                  |        |  |  |  allocate_brtag
            //     |  |  |  |  micro-code         func unit        |       |       rs3 type|  |     |  |  |  is_fence             |        |  |  |  |
            //     |  |  |  |  |         iq-type  |                |       |       |       |  |     |  |  |  |  is_fencei         |        |  |  |  |
            //     |  |  |  |  |         |        |        dst     |       |       |       |  |     |  |  |  |  |  mem    mem     |        |  |  |  |  is unique? (clear pipeline for it)
            //     |  |  |  |  |         |        |        regtype |       |       |       |  |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  flush on commit
            //     |  |  |  |  |         |        |        |       |       |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  csr cmd
   val table: Array[(BitPat, List[BitPat])] = Array(//     |       |       |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  |
   LD      -> List(Y, N, X, N, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_D , UInt(3), N, N, N, N, N, N, CSR.N),
   LW      -> List(Y, N, X, N, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_W , UInt(3), N, N, N, N, N, N, CSR.N),
   LWU     -> List(Y, N, X, N, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_WU, UInt(3), N, N, N, N, N, N, CSR.N),
   LH      -> List(Y, N, X, N, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_H , UInt(3), N, N, N, N, N, N, CSR.N),
   LHU     -> List(Y, N, X, N, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_HU, UInt(3), N, N, N, N, N, N, CSR.N),
   LB      -> List(Y, N, X, N, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_B , UInt(3), N, N, N, N, N, N, CSR.N),
   LBU     -> List(Y, N, X, N, uopLD   , IQT_MEM, FU_MEM , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_BU, UInt(3), N, N, N, N, N, N, CSR.N),

   SD      -> List(Y, N, X, N, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_S, N, Y, N, N, N, M_XWR, MSK_D , UInt(0), N, N, N, N, N, N, CSR.N),
   SW      -> List(Y, N, X, N, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_S, N, Y, N, N, N, M_XWR, MSK_W , UInt(0), N, N, N, N, N, N, CSR.N),
   SH      -> List(Y, N, X, N, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_S, N, Y, N, N, N, M_XWR, MSK_H , UInt(0), N, N, N, N, N, N, CSR.N),
   SB      -> List(Y, N, X, N, uopSTA  , IQT_MEM, FU_MEM , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_S, N, Y, N, N, N, M_XWR, MSK_B , UInt(0), N, N, N, N, N, N, CSR.N),

   LUI     -> List(Y, N, X, N, uopLUI  , IQT_INT, FU_ALU , RT_FIX, RT_X  , RT_X  , RT_NAO, N, IS_U, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   ADDI    -> List(Y, N, X, N, uopADDI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   ANDI    -> List(Y, N, X, N, uopANDI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   ORI     -> List(Y, N, X, N, uopORI  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   XORI    -> List(Y, N, X, N, uopXORI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLTI    -> List(Y, N, X, N, uopSLTI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLTIU   -> List(Y, N, X, N, uopSLTIU, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLLI    -> List(Y, N, X, N, uopSLLI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRAI    -> List(Y, N, X, N, uopSRAI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRLI    -> List(Y, N, X, N, uopSRLI , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   ADDIW   -> List(Y, N, X, N, uopADDIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLLIW   -> List(Y, N, X, N, uopSLLIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRAIW   -> List(Y, N, X, N, uopSRAIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRLIW   -> List(Y, N, X, N, uopSRLIW, IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   SLL     -> List(Y, N, X, N, uopSLL  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   ADD     -> List(Y, N, X, N, uopADD  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SUB     -> List(Y, N, X, N, uopSUB  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLT     -> List(Y, N, X, N, uopSLT  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLTU    -> List(Y, N, X, N, uopSLTU , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   AND     -> List(Y, N, X, N, uopAND  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   OR      -> List(Y, N, X, N, uopOR   , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   XOR     -> List(Y, N, X, N, uopXOR  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRA     -> List(Y, N, X, N, uopSRA  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRL     -> List(Y, N, X, N, uopSRL  , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   ADDW    -> List(Y, N, X, N, uopADDW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SUBW    -> List(Y, N, X, N, uopSUBW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SLLW    -> List(Y, N, X, N, uopSLLW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRAW    -> List(Y, N, X, N, uopSRAW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),
   SRLW    -> List(Y, N, X, N, uopSRLW , IQT_INT, FU_ALU , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(1), Y, N, N, N, N, N, CSR.N),

   MUL     -> List(Y, N, X, N, uopMUL  , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   MULH    -> List(Y, N, X, N, uopMULH , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   MULHU   -> List(Y, N, X, N, uopMULHU, IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   MULHSU  -> List(Y, N, X, N, uopMULHSU,IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   MULW    -> List(Y, N, X, N, uopMULW , IQT_INT, FU_MUL , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   DIV     -> List(Y, N, X, N, uopDIV  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   DIVU    -> List(Y, N, X, N, uopDIVU , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   REM     -> List(Y, N, X, N, uopREM  , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   REMU    -> List(Y, N, X, N, uopREMU , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   DIVW    -> List(Y, N, X, N, uopDIVW , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   DIVUW   -> List(Y, N, X, N, uopDIVUW, IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   REMW    -> List(Y, N, X, N, uopREMW , IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   REMUW   -> List(Y, N, X, N, uopREMUW, IQT_INT, FU_DIV , RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   AUIPC   -> List(Y, N, X, N, uopAUIPC, IQT_INT, FU_BRU , RT_FIX, RT_X  , RT_X  , RT_NAO, N, IS_U, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, N, N, N, N, N, CSR.N), // use BRU for the PC read
   JAL     -> List(Y, N, X, N, uopJAL  , IQT_INT, FU_BRU , RT_FIX, RT_X  , RT_X  , RT_NAO, N, IS_J, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, Y, Y, N, N, N, CSR.N),
   JALR    -> List(Y, N, X, N, uopJALR , IQT_INT, FU_BRU , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(1), N, Y, N, Y, N, N, CSR.N),
   BEQ     -> List(Y, N, X, N, uopBEQ  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BNE     -> List(Y, N, X, N, uopBNE  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BGE     -> List(Y, N, X, N, uopBGE  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BGEU    -> List(Y, N, X, N, uopBGEU , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BLT     -> List(Y, N, X, N, uopBLT  , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),
   BLTU    -> List(Y, N, X, N, uopBLTU , IQT_INT, FU_BRU , RT_X  , RT_FIX, RT_FIX, RT_NAO, N, IS_B, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, Y, N, Y, N, N, CSR.N),

   // I-type, the immediate12 holds the CSR register.
   CSRRW   -> List(Y, N, X, N, uopCSRRW, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.W),
   CSRRS   -> List(Y, N, X, N, uopCSRRS, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.S),
   CSRRC   -> List(Y, N, X, N, uopCSRRC, IQT_INT, FU_CSR , RT_FIX, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.C),

   CSRRWI  -> List(Y, N, X, N, uopCSRRWI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.W),
   CSRRSI  -> List(Y, N, X, N, uopCSRRSI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.S),
   CSRRCI  -> List(Y, N, X, N, uopCSRRCI,IQT_INT, FU_CSR , RT_FIX, RT_PAS, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.C),

   SFENCE_VM->List(Y, N, X, N, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   SCALL   -> List(Y, N, X, N, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   SBREAK  -> List(Y, N, X, N, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   SRET    -> List(Y, N, X, N, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   MRET    -> List(Y, N, X, N, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),
   DRET    -> List(Y, N, X, N, uopSYSTEM,IQT_INT, FU_CSR , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, N, CSR.I),

   WFI     -> List(Y, N, X, N, uopNOP   ,IQT_INT, FU_X   , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.N), // implemented as a NOP; TODO

   FENCE_I -> List(Y, N, X, N, uopNOP  , IQT_INT, FU_X   , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_X, N, N, N, N, Y, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.N),
   FENCE   -> List(Y, N, X, N, uopFENCE, IQT_INT, FU_MEM , RT_X  , RT_X  , RT_X  , RT_NAO, N, IS_X, N, Y, N, Y, N, M_X  , MSK_X , UInt(0), N, N, N, N, Y, Y, CSR.N), // TODO PERF make fence higher performance
                                                                                                                                                   // currently serializes pipeline
   // A-type
   AMOADD_W-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_ADD, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N), // TODO make AMOs higherperformance
   AMOXOR_W-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_XOR, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOSWAP_W->List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOAND_W-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_AND, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOOR_W -> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_OR,  MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMIN_W-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_MIN, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMINU_W->List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_MINU,MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMAX_W-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_MAX, MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMAXU_W->List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N),

   AMOADD_D-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_ADD, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOXOR_D-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_XOR, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOSWAP_D->List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_SWAP,MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOAND_D-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_AND, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOOR_D -> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_OR,  MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMIN_D-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_MIN, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMINU_D->List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_MINU,MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMAX_D-> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_MAX, MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),
   AMOMAXU_D->List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XA_MAXU,MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N),

   LR_W    -> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XLR   , MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N), // TODO optimize LR, SC
   LR_D    -> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XLR   , MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N), // note LR generates 2 micro-ops,
   SC_W    -> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XSC   , MSK_W,UInt(0),N, N, N, N, Y, Y, CSR.N), // one which isn't needed
   SC_D    -> List(Y, N, X, N, uopAMO_AG, IQT_MEM, FU_MEM, RT_FIX, RT_FIX, RT_FIX, RT_NAO, N, IS_X, N, Y, Y, N, N, M_XSC   , MSK_D,UInt(0),N, N, N, N, Y, Y, CSR.N)
   )
// scalastyle:on
}

object FDecode extends DecodeConstants
{
// scalastyle:off
  val table: Array[(BitPat, List[BitPat])] = Array(
             //                                                                             frs3_en                                wakeup_delay
             //                                                                             |  imm sel                             |        bypassable (aka, known/fixed latency)
             //                                                                             |  |     is_load                       |        |  br/jmp
             //    is val inst?                                     rs1 regtype             |  |     |  is_store                   |        |  |  is jal
             //    |  is fp inst?                                   |       rs2 type        |  |     |  |  is_amo                  |        |  |  |  allocate_brtag
             //    |  |  is dst single-prec?                        |       |       rs3 type|  |     |  |  |  is_fence             |        |  |  |  |
             //    |  |  |  is hfp inst?                            |       |       |       |  |     |  |  |  |  is_fencei         |        |  |  |  |
             //    |  |  |  |  micro-opcode         func    dst     |       |       |       |  |     |  |  |  |  |  mem    mem     |        |  |  |  |  is unique? (clear pipeline for it)
             //    |  |  |  |  |           iq_type  unit    regtype |       |       |       |  |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  flush on commit
             //    |  |  |  |  |           |        |       |       |       |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  csr cmd
   FLW     -> List(Y, Y, Y, N, uopLD     , IQT_MEM, FU_MEM, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_W , UInt(0), N, N, N, N, N, N, CSR.N),
   FLD     -> List(Y, Y, N, N, uopLD     , IQT_MEM, FU_MEM, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_D , UInt(0), N, N, N, N, N, N, CSR.N),
   FSW     -> List(Y, Y, Y, N, uopSTA    , IQT_MEM, FU_MEM, RT_X  , RT_FIX, RT_FLT, RT_NAO, N, IS_S, N, Y, N, N, N, M_XWR, MSK_W , UInt(0), N, N, N, N, N, N, CSR.N), // sort of a lie; broken into two micro-ops
   FSD     -> List(Y, Y, N, N, uopSTA    , IQT_MEM, FU_MEM, RT_X  , RT_FIX, RT_FLT, RT_NAO, N, IS_S, N, Y, N, N, N, M_XWR, MSK_D , UInt(0), N, N, N, N, N, N, CSR.N),

   FCLASS_S-> List(Y, Y, Y, N, uopFCLASS_S,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCLASS_D-> List(Y, Y, N, N, uopFCLASS_D,IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMV_S_X -> List(Y, Y, Y, N, uopFMV_S_X, IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_D_X -> List(Y, Y, N, N, uopFMV_D_X, IQT_INT, FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_X_S -> List(Y, Y, Y, N, uopFMV_X_S, IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_X_D -> List(Y, Y, N, N, uopFMV_X_D, IQT_FP , FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FSGNJ_S -> List(Y, Y, Y, N, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJ_D -> List(Y, Y, N, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJX_S-> List(Y, Y, Y, N, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJX_D-> List(Y, Y, N, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJN_S-> List(Y, Y, Y, N, uopFSGNJ_S, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJN_D-> List(Y, Y, N, N, uopFSGNJ_D, IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // FP to FP
   FCVT_S_D-> List(Y, Y, Y, N, uopFCVT_S_D,IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_S-> List(Y, Y, N, N, uopFCVT_D_S,IQT_FP , FU_FPU, RT_FLT, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // Int to FP
   FCVT_S_W-> List(Y, Y, Y, N, uopFCVT_S_W ,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_S_WU->List(Y, Y, Y, N, uopFCVT_S_WU,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_S_L-> List(Y, Y, Y, N, uopFCVT_S_L ,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_S_LU->List(Y, Y, Y, N, uopFCVT_S_LU,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FCVT_D_W-> List(Y, Y, N, N, uopFCVT_D_W ,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_WU->List(Y, Y, N, N, uopFCVT_D_WU,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_L-> List(Y, Y, N, N, uopFCVT_D_L ,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_LU->List(Y, Y, N, N, uopFCVT_D_LU,IQT_INT,FU_I2F, RT_FLT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // FP to Int
   FCVT_W_S-> List(Y, Y, Y, N, uopFCVT_W_S ,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_WU_S->List(Y, Y, Y, N, uopFCVT_WU_S,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_L_S-> List(Y, Y, Y, N, uopFCVT_L_S ,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_LU_S->List(Y, Y, Y, N, uopFCVT_LU_S,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FCVT_W_D-> List(Y, Y, N, N, uopFCVT_W_D ,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_WU_D->List(Y, Y, N, N, uopFCVT_WU_D,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_L_D-> List(Y, Y, N, N, uopFCVT_L_D ,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_LU_D->List(Y, Y, N, N, uopFCVT_LU_D,IQT_FP, FU_F2I, RT_FIX, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // "fp_single" is used for wb_data formatting (and debugging)
   FEQ_S    ->List(Y, Y, Y, N, uopFEQ_S  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLT_S    ->List(Y, Y, Y, N, uopFLT_S  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLE_S    ->List(Y, Y, Y, N, uopFLE_S  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FEQ_D    ->List(Y, Y, N, N, uopFEQ_D  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLT_D    ->List(Y, Y, N, N, uopFLT_D  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLE_D    ->List(Y, Y, N, N, uopFLE_D  , IQT_FP,  FU_F2I, RT_FIX, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMIN_S   ->List(Y, Y, Y, N, uopFMIN_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMAX_S   ->List(Y, Y, Y, N, uopFMAX_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMIN_D   ->List(Y, Y, N, N, uopFMIN_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMAX_D   ->List(Y, Y, N, N, uopFMAX_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FADD_S   ->List(Y, Y, Y, N, uopFADD_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSUB_S   ->List(Y, Y, Y, N, uopFSUB_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMUL_S   ->List(Y, Y, Y, N, uopFMUL_S , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FADD_D   ->List(Y, Y, N, N, uopFADD_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSUB_D   ->List(Y, Y, N, N, uopFSUB_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMUL_D   ->List(Y, Y, N, N, uopFMUL_D , IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMADD_S  ->List(Y, Y, Y, N, uopFMADD_S, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMSUB_S  ->List(Y, Y, Y, N, uopFMSUB_S, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMADD_S ->List(Y, Y, Y, N, uopFNMADD_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMSUB_S ->List(Y, Y, Y, N, uopFNMSUB_S,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMADD_D  ->List(Y, Y, N, N, uopFMADD_D, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMSUB_D  ->List(Y, Y, N, N, uopFMSUB_D, IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMADD_D ->List(Y, Y, N, N, uopFNMADD_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMSUB_D ->List(Y, Y, N, N, uopFNMSUB_D,IQT_FP,  FU_FPU, RT_FLT, RT_FLT, RT_FLT, RT_FLT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N)
   )

// scalastyle:on
}

object FDivSqrtDecode extends DecodeConstants
{
// scalastyle:off
  val table: Array[(BitPat, List[BitPat])] = Array(
             //                                                                             frs3_en                                wakeup_delay
             //                                                                             |  imm sel                             |        bypassable (aka, known/fixed latency)
             //                                                                             |  |     is_load                       |        |  br/jmp
             //     is val inst?                                    rs1 regtype             |  |     |  is_store                   |        |  |  is jal
             //     |  is fp inst?                                  |       rs2 type        |  |     |  |  is_amo                  |        |  |  |  allocate_brtag
             //     |  |  is dst single-prec?                       |       |       rs3 type|  |     |  |  |  is_fence             |        |  |  |  |
             //     |  |  |  is hfp inst?                           |       |       |       |  |     |  |  |  |  is_fencei         |        |  |  |  |
             //     |  |  |  |  micro-opcode        func    dst     |       |       |       |  |     |  |  |  |  |  mem    mem     |        |  |  |  |  is unique? (clear pipeline for it)
             //     |  |  |  |  |          iq-type  unit    regtype |       |       |       |  |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  flush on commit
             //     |  |  |  |  |          |        |       |       |       |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  csr cmd
   FDIV_S    ->List(Y, Y, Y, N, uopFDIV_S , IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FDIV_D    ->List(Y, Y, N, N, uopFDIV_D , IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_FLT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSQRT_S   ->List(Y, Y, Y, N, uopFSQRT_S, IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_X  , RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSQRT_D   ->List(Y, Y, N, N, uopFSQRT_D, IQT_FP, FU_FDV, RT_FLT, RT_FLT, RT_X  , RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N)
   )
// scalastyle:on
}

object HFDecode extends DecodeConstants
{
// scalastyle:off
  val table: Array[(BitPat, List[BitPat])] = Array(
             //                                                                               frs3_en                                wakeup_delay
             //                                                                               |  imm sel                             |        bypassable (aka, known/fixed latency)
             //                                                                               |  |     is_load                       |        |  br/jmp
             //    is val inst?                                       rs1 regtype             |  |     |  is_store                   |        |  |  is jal
             //    |  is fp inst?                                     |       rs2 type        |  |     |  |  is_amo                  |        |  |  |  allocate_brtag
             //    |  |  is dst single-prec?                          |       |       rs3 type|  |     |  |  |  is_fence             |        |  |  |  |
             //    |  |  |  is hfp inst?                              |       |       |       |  |     |  |  |  |  is_fencei         |        |  |  |  |
             //    |  |  |  |  micro-opcode          func     dst     |       |       |       |  |     |  |  |  |  |  mem    mem     |        |  |  |  |  is unique? (clear pipeline for it)
             //    |  |  |  |  |           iq_type   unit     regtype |       |       |       |  |     |  |  |  |  |  cmd    msk     |        |  |  |  |  |  flush on commit
             //    |  |  |  |  |           |         |        |       |       |       |       |  |     |  |  |  |  |  |      |       |        |  |  |  |  |  |  csr cmd
   FLH     -> List(Y, N, N, Y, uopLD     , IQT_MEM,  FU_MEM,  RT_FHT, RT_FIX, RT_X  , RT_NAO, N, IS_I, Y, N, N, N, N, M_XRD, MSK_H , UInt(0), N, N, N, N, N, N, CSR.N),
   FSH     -> List(Y, N, N, Y, uopSTA    , IQT_MEM,  FU_MEM,  RT_X  , RT_FIX, RT_FHT, RT_NAO, N, IS_S, N, Y, N, N, N, M_XWR, MSK_H , UInt(0), N, N, N, N, N, N, CSR.N),

   FCLASS_H-> List(Y, N, N, Y, uopFCLASS_H,IQT_HFP,  FU_HFPU, RT_FIX, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMV_H_X -> List(Y, N, N, Y, uopFMV_H_X, IQT_INT,  FU_I2HF, RT_FHT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_X_H -> List(Y, N, N, Y, uopFMV_X_H, IQT_HFP,  FU_HF2I, RT_FIX, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_H_F -> List(Y, N, N, Y, uopFMV_H_F, IQT_FP ,  FU_F2HF, RT_FHT, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMV_F_H -> List(Y, N, N, Y, uopFMV_F_H, IQT_HFP,  FU_HF2F, RT_FLT, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FSGNJ_H -> List(Y, N, N, Y, uopFSGNJ_H, IQT_HFP,  FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJX_H-> List(Y, N, N, Y, uopFSGNJ_H, IQT_HFP,  FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSGNJN_H-> List(Y, N, N, Y, uopFSGNJ_H, IQT_HFP,  FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // FP to HFP or HFP to FP
   FCVT_H_D-> List(Y, N, N, Y, uopFCVT_H_D,IQT_FP ,  FU_F2HF, RT_FHT, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_H_S-> List(Y, N, N, Y, uopFCVT_H_S,IQT_FP ,  FU_F2HF, RT_FHT, RT_FLT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_S_H-> List(Y, Y, N, Y, uopFCVT_S_H,IQT_HFP,  FU_HF2F, RT_FLT, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_D_H-> List(Y, N, N, Y, uopFCVT_D_H,IQT_HFP,  FU_HF2F, RT_FLT, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // Int to HFP
   FCVT_H_W-> List(Y, N, N, Y, uopFCVT_H_W ,IQT_INT, FU_I2HF, RT_FHT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_H_WU->List(Y, N, N, Y, uopFCVT_H_WU,IQT_INT, FU_I2HF, RT_FHT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_H_L-> List(Y, N, N, Y, uopFCVT_H_L ,IQT_INT, FU_I2HF, RT_FHT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_H_LU->List(Y, N, N, Y, uopFCVT_H_LU,IQT_INT, FU_I2HF, RT_FHT, RT_FIX, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   // HFP to Int
   FCVT_W_H-> List(Y, N, N, Y, uopFCVT_W_H ,IQT_HFP, FU_HF2I, RT_FIX, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_WU_H->List(Y, N, N, Y, uopFCVT_WU_H,IQT_HFP, FU_HF2I, RT_FIX, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_L_H-> List(Y, N, N, Y, uopFCVT_L_H ,IQT_HFP, FU_HF2I, RT_FIX, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FCVT_LU_H->List(Y, N, N, Y, uopFCVT_LU_H,IQT_HFP, FU_HF2I, RT_FIX, RT_FHT, RT_X  , RT_NAO, N, IS_I, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FEQ_H    ->List(Y, N, N, Y, uopFEQ_H   , IQT_HFP, FU_HFPU, RT_FIX, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLT_H    ->List(Y, N, N, Y, uopFLT_H   , IQT_HFP, FU_HFPU, RT_FIX, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FLE_H    ->List(Y, N, N, Y, uopFLE_H   , IQT_HFP, FU_HFPU, RT_FIX, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMIN_H   ->List(Y, N, N, Y, uopFMIN_H  , IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMAX_H   ->List(Y, N, N, Y, uopFMAX_H  , IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FADD_H   ->List(Y, N, N, Y, uopFADD_H  , IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FSUB_H   ->List(Y, N, N, Y, uopFSUB_H  , IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMUL_H   ->List(Y, N, N, Y, uopFMUL_H  , IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_NAO, N, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),

   FMADD_H  ->List(Y, N, N, Y, uopFMADD_H , IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_FHT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FMSUB_H  ->List(Y, N, N, Y, uopFMSUB_H , IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_FHT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMADD_H ->List(Y, N, N, Y, uopFNMADD_H, IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_FHT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N),
   FNMSUB_H ->List(Y, N, N, Y, uopFNMSUB_H, IQT_HFP, FU_HFPU, RT_FHT, RT_FHT, RT_FHT, RT_FHT, Y, IS_X, N, N, N, N, N, M_X  , MSK_X , UInt(0), N, N, N, N, N, N, CSR.N)
   )

// scalastyle:on
}



class DecodeUnitIo(implicit p: Parameters) extends BoomBundle()(p)
{
   val enq = new Bundle { val uop = new MicroOp().asInput }
   val deq = new Bundle { val uop = new MicroOp().asOutput }

   // from CSRFile
   val status = new rocket.MStatus().asInput
   val interrupt = Bool(INPUT)
   val interrupt_cause = UInt(INPUT, xLen)

   override def cloneType: this.type = new DecodeUnitIo()(p).asInstanceOf[this.type]
}

// Takes in a single instruction, generates a MicroOp (or multiply micro-ops over x cycles)
class DecodeUnit(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new DecodeUnitIo)

   val uop = Wire(new MicroOp())
   uop := io.enq.uop

   var decode_table = XDecode.table
   if (usingFPU) decode_table ++= FDecode.table
   if (usingFPU && usingFDivSqrt) decode_table ++= FDivSqrtDecode.table
   if (usingHFPU) decode_table ++= HFDecode.table

   val cs = Wire(new CtrlSigs()).decode(uop.inst, decode_table)

   // Exception Handling
   val id_illegal_insn = !cs.legal ||
      cs.fp_val && !io.status.fs.orR ||
      cs.rocc && !io.status.xs.orR

   def checkExceptions(x: Seq[(Bool, UInt)]) =
      (x.map(_._1).reduce(_||_), PriorityMux(x))

   val (xcpt_valid, xcpt_cause) = checkExceptions(List(
      (io.interrupt,     io.interrupt_cause),
      (uop.replay_if,    MINI_EXCEPTION_REPLAY),
      (uop.xcpt_pf_if,   UInt(Causes.fault_fetch)),
      (uop.xcpt_ma_if,   UInt(Causes.misaligned_fetch)),
      (id_illegal_insn,  UInt(Causes.illegal_instruction))))

   uop.exception := xcpt_valid
   uop.exc_cause := xcpt_cause

   //-------------------------------------------------------------

   uop.uopc       := cs.uopc
   uop.iqtype     := cs.iqtype
   uop.fu_code    := cs.fu_code

   // x-registers placed in 0-31, f-registers placed in 32-63.
   // This allows us to straight-up compare register specifiers and not need to
   // verify the rtypes (e.g., bypassing in rename).
   uop.ldst       := uop.inst(RD_MSB,RD_LSB)
   uop.lrs1       := uop.inst(RS1_MSB,RS1_LSB)
   uop.lrs2       := uop.inst(RS2_MSB,RS2_LSB)
   uop.lrs3       := uop.inst(RS3_MSB,RS3_LSB)

   uop.ldst_val   := cs.dst_type =/= RT_X && !(uop.ldst === UInt(0) && uop.dst_rtype === RT_FIX)
   uop.dst_rtype  := cs.dst_type
   uop.lrs1_rtype := cs.rs1_type
   uop.lrs2_rtype := cs.rs2_type
   uop.frs3_en    := cs.frs3_en

   uop.fp_val     := cs.fp_val
   uop.fp_single  := cs.fp_single // TODO use this signal instead of the FPU decode's table signal?

   uop.mem_cmd    := cs.mem_cmd
   uop.mem_typ    := cs.mem_typ
   uop.is_load    := cs.is_load
   uop.is_store   := cs.is_store
   uop.is_amo     := cs.is_amo
   uop.is_fence   := cs.is_fence
   uop.is_fencei  := cs.is_fencei
   uop.is_unique  := cs.inst_unique
   uop.flush_on_commit := cs.flush_on_commit

   uop.bypassable   := cs.bypassable

   //-------------------------------------------------------------
   // immediates

   // repackage the immediate, and then pass the fewest number of bits around
   val di24_20 = Mux(cs.imm_sel === IS_B || cs.imm_sel === IS_S, uop.inst(11,7), uop.inst(24,20))
   uop.imm_packed := Cat(uop.inst(31,25), di24_20, uop.inst(19,12))

   //-------------------------------------------------------------

   uop.allocate_brtag := cs.allocate_brtag
   uop.is_br_or_jmp   := cs.br_or_jmp
   uop.is_jal         := cs.is_jal
   uop.is_jump        := cs.is_jal || (uop.uopc === uopJALR)
   uop.is_ret         := (uop.uopc === uopJALR) &&
                         (uop.ldst === X0) &&
                         (uop.lrs1 === RA)
   uop.is_call        := (uop.uopc === uopJALR || uop.uopc === uopJAL) &&
                         (uop.ldst === RA)

   //-------------------------------------------------------------

   io.deq.uop := uop

   //-------------------------------------------------------------
}


class BranchDecode extends Module
{
   val io = IO(new Bundle
   {
      val inst    = UInt(INPUT, 32)
      val is_br   = Bool(OUTPUT)
      val is_jal  = Bool(OUTPUT)
      val is_jalr = Bool(OUTPUT)
   })

   val bpd_csignals =
      rocket.DecodeLogic(io.inst,
                  List[BitPat](N, N, N, IS_X),
////                      //   is br?
////                      //   |  is jal?
////                      //   |  |  is jalr?
////                      //   |  |  |  br type
////                      //   |  |  |  |
            Array[(BitPat, List[BitPat])](
               JAL     -> List(N, Y, N, IS_J),
               JALR    -> List(N, N, Y, IS_I),
               BEQ     -> List(Y, N, N, IS_B),
               BNE     -> List(Y, N, N, IS_B),
               BGE     -> List(Y, N, N, IS_B),
               BGEU    -> List(Y, N, N, IS_B),
               BLT     -> List(Y, N, N, IS_B),
               BLTU    -> List(Y, N, N, IS_B)
            ))

   val (cs_is_br: Bool) :: (cs_is_jal: Bool) :: (cs_is_jalr:Bool) :: imm_sel_ :: Nil = bpd_csignals

   io.is_br   := cs_is_br
   io.is_jal  := cs_is_jal
   io.is_jalr := cs_is_jalr
}


class FetchSerializerResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val uops = Vec(DECODE_WIDTH, new MicroOp())
   val bpd_resp = new BpdResp()
}
class FetchSerializerIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val enq = new DecoupledIO(new FetchBundle()).flip
   val deq = new DecoupledIO(new FetchSerializerResp)
   val kill = Bool(INPUT)
}


// TODO horrific hodgepodge, needs refactoring
// connect a N-word wide Fetch Buffer with a M-word decode
// currently only works for 2 wide fetch to 1 wide decode, OR N:N fetch/decode
// TODO instead of counter, clear mask bits as instructions are finished?
class FetchSerializerNtoM(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new FetchSerializerIO)

   val counter = Reg(init = UInt(0, log2Up(FETCH_WIDTH)))
   val inst_idx = Wire(UInt())
   inst_idx := UInt(0)

   //-------------------------------------------------------------
   // Compute index for where to get the instruction
   when (counter === UInt(1))
   {
      inst_idx := UInt(1)
   }
   .otherwise
   {
      inst_idx := Mux(io.enq.bits.mask === UInt(2), UInt(1), UInt(0))
   }

   //-------------------------------------------------------------
   // Compute Enqueue Ready (get the next bundle)
   io.enq.ready := io.deq.ready &&
                     (io.enq.bits.mask =/= UInt(3) || (counter === UInt(1)))


   //-------------------------------------------------------------
   // Compute Counter
   when (io.kill || io.enq.ready)
   {
      // reset counter on every new bundle
      counter := UInt(0)
   }
   .elsewhen (io.deq.valid && io.deq.ready)
   {
      counter := counter + UInt(1)
   }


   //-------------------------------------------------------------
   // override all the above logic for FW==1
   if (FETCH_WIDTH == 1)
   {
      inst_idx := UInt(0)
      io.enq.ready := io.deq.ready
   }

   io.deq.bits.uops(0).pc             := io.enq.bits.pc
   io.deq.bits.uops(0).fetch_pc_lob   := io.enq.bits.pc
   io.deq.bits.uops(0).inst           := io.enq.bits.insts(inst_idx)
   io.deq.bits.uops(0).br_prediction  := io.enq.bits.bpu_info(inst_idx)
   io.deq.bits.uops(0).valid          := io.enq.bits.mask(inst_idx)
   io.deq.bits.uops(0).xcpt_pf_if     := io.enq.bits.xcpt_pf_if
   io.deq.bits.uops(0).replay_if      := io.enq.bits.replay_if
   io.deq.bits.uops(0).xcpt_ma_if     := io.enq.bits.xcpt_ma_if_oh(inst_idx)
   io.deq.bits.uops(0).debug_events   := io.enq.bits.debug_events(inst_idx)

   //-------------------------------------------------------------
   // override all the above logic for DW>1
   // assume FW is also DW, and pass everything through
   if ((DECODE_WIDTH == FETCH_WIDTH) && (FETCH_WIDTH > 1))
   {
      // 1:1, so pass everything straight through!
      for (i <- 0 until DECODE_WIDTH)
      {
         io.deq.bits.uops(i).valid          := io.enq.bits.mask(i)
         io.deq.bits.uops(i).pc             := (io.enq.bits.pc.asSInt & SInt(-(FETCH_WIDTH*coreInstBytes))).asUInt + UInt(i << 2)
         io.deq.bits.uops(i).fetch_pc_lob   := io.enq.bits.pc
         io.deq.bits.uops(i).inst           := io.enq.bits.insts(i)
         io.deq.bits.uops(i).xcpt_pf_if     := io.enq.bits.xcpt_pf_if
         io.deq.bits.uops(i).replay_if      := io.enq.bits.replay_if
         io.deq.bits.uops(i).xcpt_ma_if     := io.enq.bits.xcpt_ma_if_oh(i)
         io.deq.bits.uops(i).br_prediction  := io.enq.bits.bpu_info(i)
         io.deq.bits.uops(i).debug_events   := io.enq.bits.debug_events(i)
      }
      io.enq.ready := io.deq.ready
   }

   // Pipe valid straight through, since conceptually,
   // we are just an extension of the Fetch Buffer
   io.deq.valid := io.enq.valid
   // TODO XXX HACK bpd_resp is associated with a "fetch packet", so only copy the first one.
   // Ideally, separate out which parts of bpu_info are "per packet"/"per instruction" and "commit info" and "exe info".
   io.deq.bits.bpd_resp := io.enq.bits.bpu_info(0).bpd_resp

}


// track the current "branch mask", and give out the branch mask to each micro-op in Decode
// (each micro-op in the machine has a branch mask which says which branches it
// is being speculated under).

class DebugBranchMaskGenerationLogicIO(implicit p: Parameters) extends BoomBundle()(p)
{
   val branch_mask = UInt(width = MAX_BR_COUNT)
}

class BranchMaskGenerationLogic(val pl_width: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      // guess if the uop is a branch (we'll catch this later)
      val is_branch = Vec(pl_width, Bool()).asInput
      // lock in that it's actually a branch and will fire, so we update
      // the branch_masks.
      val will_fire = Vec(pl_width, Bool()).asInput

      // give out tag immediately (needed in rename)
      // mask can come later in the cycle
      val br_tag    = Vec(pl_width, UInt(width=BR_TAG_SZ)).asOutput
      val br_mask   = Vec(pl_width, UInt(width=MAX_BR_COUNT)).asOutput

       // tell decoders the branch mask has filled up, but on the granularity
       // of an individual micro-op (so some micro-ops can go through)
      val is_full   = Vec(pl_width, Bool()).asOutput

      val brinfo         = new BrResolutionInfo().asInput
      val flush_pipeline = Bool(INPUT)

      val debug = new DebugBranchMaskGenerationLogicIO().asOutput
   })

   val branch_mask = Reg(init = UInt(0, MAX_BR_COUNT))

   //-------------------------------------------------------------
   // Give out the branch tag to each branch micro-op

   var allocate_mask = branch_mask
   val tag_masks = Wire(Vec(pl_width, UInt(width=MAX_BR_COUNT)))

   for (w <- 0 until pl_width)
   {
      // TODO this is a loss of performance as we're blocking branches based on potentially fake branches
      io.is_full(w) := (allocate_mask === ~(UInt(0,MAX_BR_COUNT))) && io.is_branch(w)

      // find br_tag and compute next br_mask
      val new_br_tag = Wire(UInt(width = BR_TAG_SZ))
      new_br_tag := UInt(0)
      tag_masks(w) := UInt(0)

      for (i <- MAX_BR_COUNT-1 to 0 by -1)
      {
         when (~allocate_mask(i))
         {
            new_br_tag := UInt(i)
            tag_masks(w) := (UInt(1) << UInt(i))
         }
      }

      io.br_tag(w) := new_br_tag
      allocate_mask = Mux(io.is_branch(w), tag_masks(w) | allocate_mask, allocate_mask)
   }

   //-------------------------------------------------------------
   // Give out the branch mask to each micro-op
   // (kill off the bits that corresponded to branches that aren't going to fire)

   var curr_mask = branch_mask
   for (w <- 0 until pl_width)
   {
      io.br_mask(w) := GetNewBrMask(io.brinfo, curr_mask)
      curr_mask = Mux(io.will_fire(w), tag_masks(w) | curr_mask, curr_mask)
   }

   //-------------------------------------------------------------
   // Update the current branch_mask

   when (io.flush_pipeline)
   {
      branch_mask := UInt(0)
   }
   .elsewhen (io.brinfo.valid && io.brinfo.mispredict)
   {
      branch_mask := io.brinfo.exe_mask
   }
   .otherwise
   {
      branch_mask := GetNewBrMask(io.brinfo, curr_mask)
   }

   io.debug.branch_mask := branch_mask
}

}
