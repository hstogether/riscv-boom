//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Execution Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio
// 2013 Apr 27
//
// The issue window schedules micro-ops onto a specific execution pipeline
// A given execution pipeline may contain multiple functional units; one or more
// read ports, and one or more writeports.

package boom

import Chisel._
import config.Parameters
import scala.collection.mutable.ArrayBuffer

import FUConstants._
import tile.XLen
import uncore.constants.MemoryOpConstants._

// TODO rename to something like MicroOpWithData
class ExeUnitResp(data_width: Int)(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val data = Bits(width = data_width)
   val fflags = new ValidIO(new FFlagsResp) // write fflags to ROB

   var writesToIRF = true // does this response unit plug into the integer regfile?
   var writesToFRF = true 
   override def cloneType: this.type = new ExeUnitResp(data_width).asInstanceOf[this.type]
}

class FFlagsResp(implicit p: Parameters) extends BoomBundle()(p)
{
   val uop = new MicroOp()
   val flags = Bits(width=tile.FPConstants.FLAGS_SZ)
}

class ExecutionUnitIO(num_rf_read_ports: Int
                     , num_rf_write_ports: Int
                     , num_bypass_ports: Int
                     , data_width: Int
                     )(implicit p: Parameters) extends BoomBundle()(p)
{
   // describe which functional units we support (used by the issue window)
   val fu_types = Bits(OUTPUT, FUC_SZ)

   val req     = (new DecoupledIO(new FuncUnitReq(data_width))).flip
   val resp    = Vec(num_rf_write_ports, new DecoupledIO(new ExeUnitResp(data_width)))
   val bypass  = new BypassData(num_bypass_ports, data_width).asOutput
   val brinfo  = new BrResolutionInfo().asInput

   // only used by the branch unit
   val br_unit = new BranchUnitResp().asOutput
   val get_rob_pc = new RobPCRequest().flip
   val get_pred = new GetPredictionInfo
   val status = new rocket.MStatus().asInput

   // only used by the fpu unit
   val fcsr_rm = Bits(INPUT, tile.FPConstants.RM_SZ)

   // only used by the mem unit
   val lsu_io = new LoadStoreUnitIO(DECODE_WIDTH).flip
   val dmem   = new DCMemPortIO() // TODO move this out of ExecutionUnit
   val com_exception = Bool(INPUT)

   val feedback = Bits(INPUT,width=10)
}

abstract class ExecutionUnit(val num_rf_read_ports: Int
                            , val num_rf_write_ports: Int
                            , val num_bypass_stages: Int
                            , val data_width: Int
                            , val num_variable_write_ports: Int = 0
                            , var bypassable: Boolean           = false // TODO make override def for code clarity
                            , val is_mem_unit: Boolean          = false
                            , var uses_csr_wport: Boolean       = false
                            , var uses_iss_unit : Boolean       = true
                            ,     is_branch_unit: Boolean       = false
                            , val has_alu       : Boolean       = false
                            , val has_fpu       : Boolean       = false
                            , val has_mul       : Boolean       = false
                            , val has_div       : Boolean       = false
                            , val has_fdiv      : Boolean       = false
                            , val has_ifpu      : Boolean       = false
                            , val has_ihfpu     : Boolean       = false
                            , val has_fpiu      : Boolean       = false
                            , val has_fphfpu    : Boolean       = false
                            , val has_hfpfpu    : Boolean       = false
                            , val has_hfpu      : Boolean       = false
                            , val has_hfdiv     : Boolean       = false
                            , val has_hfpiu     : Boolean       = false
                            )(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new ExecutionUnitIO(num_rf_read_ports, num_rf_write_ports
                               , num_bypass_stages, data_width))

   io.resp.map(_.bits.fflags.valid := Bool(false))
   //val feedback = Wire(Bits(0,10))
   //io.feedback := UInt(0,10)

   // TODO add "number of fflag ports", so we can properly account for FPU+Mem combinations
   def numBypassPorts: Int = num_bypass_stages
   def hasBranchUnit : Boolean = is_branch_unit
   def isBypassable  : Boolean = bypassable
   def hasFFlags     : Boolean = has_fpu || has_fdiv || has_hfpu || has_hfdiv
   def usesFRF       : Boolean = (has_fpu || has_fdiv) && !(has_alu || has_mul)
   def usesIRF       : Boolean = !(has_fpu || has_fdiv || has_hfpu) && (has_alu || has_mul || is_mem_unit || has_ifpu || has_ihfpu)

   require ((has_fpu || has_fdiv || has_hfpu || has_fphfpu) ^ (has_alu || has_mul || is_mem_unit || has_ifpu || has_ihfpu),
      "[execute] we no longer support mixing FP/HFP and Integer functional units in the same exe unit.")

   def supportedFuncUnits =
   {
      new SupportedFuncUnits(
         alu = has_alu,
         bru = is_branch_unit,
         mem = is_mem_unit,
         muld = has_mul || has_div,
         fpu = has_fpu,
         csr = uses_csr_wport,
         fdiv = has_fdiv,
         ifpu = has_ifpu,
         ihfpu = has_ihfpu,
         fphfpu = has_fphfpu,
         hfdiv = has_hfdiv,
         hfpu = has_hfpu)
   }
}

class ALUExeUnit(
   is_branch_unit  : Boolean = false,
   shares_csr_wport: Boolean = false,
   has_alu         : Boolean = true,
   has_fpu         : Boolean = false,
   has_mul         : Boolean = false,
   has_div         : Boolean = false,
   has_fdiv        : Boolean = false,
   has_ifpu        : Boolean = false,
   has_ihfpu       : Boolean = false,
   use_slow_mul    : Boolean = false)
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = if (has_fpu) 3 else 2,
      num_rf_write_ports = 1,
      num_bypass_stages =
         (if (has_fpu && has_alu) p(tile.TileKey).core.fpu.get.dfmaLatency
         else if (has_alu && has_mul && !use_slow_mul) p(BoomKey).imulLatency
         else if (has_alu) 1 else 0),
      data_width = if (has_fpu || has_fdiv) 65 else 64,
      bypassable = has_alu,
      is_mem_unit = false,
      uses_csr_wport = shares_csr_wport,
      is_branch_unit = is_branch_unit,
      has_alu  = has_alu,
      has_fpu  = has_fpu,
      has_mul  = has_mul,
      has_div  = has_div,
      has_fdiv = has_fdiv,
      has_ifpu = has_ifpu,
      has_ihfpu= has_ihfpu)(p)
{
   val has_muldiv = has_div || (has_mul && use_slow_mul)

   println ("     ExeUnit--")
   if (has_alu) println ("       - ALU")
   if (has_fpu) println ("       - FPU (Latency: " + dfmaLatency + ")")
   if (has_mul && !use_slow_mul) println ("       - Mul (pipelined)")
   if (has_div && has_mul && use_slow_mul) println ("       - Mul/Div (unpipelined)")
   else if (has_mul && use_slow_mul) println ("       - Mul (unpipelined)")
   else if (has_div) println ("       - Div")
   if (has_fdiv) println ("       - FDiv/FSqrt")
   if (has_ifpu || has_ihfpu) println ("       - IFPU/IHFPU (for read port access)")

   val muldiv_busy = Wire(init=Bool(false))
   val fdiv_busy = Wire(init=Bool(false))

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := FU_ALU |
                  Mux(Bool(has_fpu), FU_FPU, Bits(0)) |
                  Mux(Bool(has_mul && !use_slow_mul), FU_MUL, Bits(0)) |
                  (Mux(!muldiv_busy && Bool(has_mul && use_slow_mul), FU_MUL, Bits(0))) |
                  (Mux(!muldiv_busy && Bool(has_div), FU_DIV, Bits(0))) |
                  (Mux(Bool(shares_csr_wport), FU_CSR, Bits(0))) |
                  (Mux(Bool(is_branch_unit), FU_BRU, Bits(0))) |
                  Mux(!fdiv_busy && Bool(has_fdiv), FU_FDV, Bits(0))


   // ALU Unit -------------------------------
   var alu: ALUUnit = null
   if (has_alu)
   {
      alu = Module(new ALUUnit(is_branch_unit = is_branch_unit, num_stages = num_bypass_stages))
      alu.io.req.valid         := io.req.valid &&
                                      (io.req.bits.uop.fu_code === FU_ALU ||
                                       io.req.bits.uop.fu_code === FU_BRU ||
                                       io.req.bits.uop.fu_code === FU_CSR)
      alu.io.req.bits.uop      := io.req.bits.uop
      alu.io.req.bits.kill     := io.req.bits.kill
      alu.io.req.bits.rs1_data := io.req.bits.rs1_data
      alu.io.req.bits.rs2_data := io.req.bits.rs2_data
      alu.io.brinfo <> io.brinfo
   }

   // branch unit is embedded inside the ALU
   if (is_branch_unit)
   {
      io.br_unit <> alu.io.br_unit
      alu.io.get_rob_pc <> io.get_rob_pc
      io.get_pred <> alu.io.get_pred
      alu.io.status <> io.status
   }
   else
   {
      io.br_unit.brinfo.valid := Bool(false)
   }

   if (has_alu) fu_units += alu

   // Pipelined, IMul Unit ------------------
   var imul: PipelinedMulUnit = null
   if (has_mul && !use_slow_mul)
   {
      imul = Module(new PipelinedMulUnit(imulLatency))
      imul.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_MUL)
      imul.io.req.bits.uop      := io.req.bits.uop
      imul.io.req.bits.rs1_data := io.req.bits.rs1_data
      imul.io.req.bits.rs2_data := io.req.bits.rs2_data
      imul.io.req.bits.kill     := io.req.bits.kill
      imul.io.brinfo <> io.brinfo
      fu_units += imul
      if (has_fpu) require (imulLatency == dfmaLatency)
   }

   // FPU Unit -----------------------
   var fpu: FPUUnit = null
   val fpu_resp_val = Wire(init=Bool(false))
   val fpu_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fpu_resp_fflags.valid := Bool(false)
   if (has_fpu)
   {
      fpu = Module(new FPUUnit())
      fpu.io.req.valid           := io.req.valid && io.req.bits.uop.fu_code_is(FU_FPU)
      fpu.io.req.bits.uop        := io.req.bits.uop
      fpu.io.req.bits.rs1_data   := io.req.bits.rs1_data
      fpu.io.req.bits.rs2_data   := io.req.bits.rs2_data
      fpu.io.req.bits.rs3_data   := io.req.bits.rs3_data
      fpu.io.req.bits.kill       := io.req.bits.kill
      fpu.io.fcsr_rm             := io.fcsr_rm
      fpu.io.brinfo <> io.brinfo
      fpu_resp_val := fpu.io.resp.valid
      fpu_resp_fflags := fpu.io.resp.bits.fflags
      fu_units += fpu
   }

   // Bypassing ------------------------------
   // (only the ALU is bypassable)

   if (has_alu) io.bypass <> alu.io.bypass

   // FDiv/FSqrt Unit -----------------------
   var fdivsqrt: FDivSqrtUnit = null
   val fdiv_resp_val = Wire(init=Bool(false))
   val fdiv_resp_uop = Wire(new MicroOp())
   val fdiv_resp_data = Wire(Bits(width=65))
   val fdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fdiv_resp_fflags.valid := Bool(false)
   if (has_fdiv)
   {
      fdivsqrt = Module(new FDivSqrtUnit())
      fdivsqrt.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV)
      fdivsqrt.io.req.bits.uop      := io.req.bits.uop
      fdivsqrt.io.req.bits.rs1_data := io.req.bits.rs1_data
      fdivsqrt.io.req.bits.rs2_data := io.req.bits.rs2_data
      fdivsqrt.io.req.bits.kill     := io.req.bits.kill
      fdivsqrt.io.fcsr_rm           := io.fcsr_rm
      fdivsqrt.io.brinfo <> io.brinfo

      // share write port with the pipelined units
      fdivsqrt.io.resp.ready := !(fu_units.map(_.io.resp.valid).reduce(_|_))

      fdiv_busy := !fdivsqrt.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV))

      fdiv_resp_val := fdivsqrt.io.resp.valid
      fdiv_resp_uop := fdivsqrt.io.resp.bits.uop
      fdiv_resp_data := fdivsqrt.io.resp.bits.data
      fdiv_resp_fflags := fdivsqrt.io.resp.bits.fflags

      fu_units += fdivsqrt
   }

   // Mul/Div/Rem Unit -----------------------
   var muldiv: MulDivUnit = null
   val muldiv_resp_val = Wire(init = Bool(false))
   if (has_muldiv)
   {
      muldiv = Module(new MulDivUnit())
      muldiv.io.req.valid           := io.req.valid &&
                                       ((io.req.bits.uop.fu_code_is(FU_DIV) && Bool(has_div)) ||
                                        (io.req.bits.uop.fu_code_is(FU_MUL) && Bool(has_mul && use_slow_mul)))
      muldiv.io.req.bits.uop        := io.req.bits.uop
      muldiv.io.req.bits.rs1_data   := io.req.bits.rs1_data
      muldiv.io.req.bits.rs2_data   := io.req.bits.rs2_data
      muldiv.io.brinfo              := io.brinfo
      muldiv.io.req.bits.kill       := io.req.bits.kill

      // share write port with the pipelined units
      muldiv.io.resp.ready := !(fu_units.map(_.io.resp.valid).reduce(_|_))

      muldiv_resp_val := muldiv.io.resp.valid
      muldiv_busy := !muldiv.io.req.ready ||
                     (io.req.valid && (io.req.bits.uop.fu_code_is(FU_DIV) ||
                                      (io.req.bits.uop.fu_code_is(FU_MUL) && Bool(has_mul && use_slow_mul))))
      fu_units += muldiv
   }

   // Outputs (Write Port #0)  ---------------

   assert (io.resp(0).ready) // don'yet support back-pressuring this unit.

   io.resp(0).valid    := fu_units.map(_.io.resp.valid).reduce(_|_)
   io.resp(0).bits.uop := new MicroOp().fromBits(
                           PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.uop.asUInt))))
   io.resp(0).bits.data:= PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   // pulled out for critical path reasons
   if (has_alu) {
      io.resp(0).bits.uop.csr_addr := ImmGen(alu.io.resp.bits.uop.imm_packed, IS_I).asUInt
      io.resp(0).bits.uop.ctrl.csr_cmd := alu.io.resp.bits.uop.ctrl.csr_cmd
   }

   io.resp(0).bits.fflags := Mux(fpu_resp_val, fpu_resp_fflags, fdiv_resp_fflags)

   assert ((PopCount(fu_units.map(_.io.resp.valid)) <= UInt(1) && !muldiv_resp_val && !fdiv_resp_val) ||
          (PopCount(fu_units.map(_.io.resp.valid)) <= UInt(2) && (muldiv_resp_val || fdiv_resp_val)) ||
          (PopCount(fu_units.map(_.io.resp.valid)) <= UInt(3) && muldiv_resp_val && fdiv_resp_val)
      , "Multiple functional units are fighting over the write port.")
}


// FPU-only unit, with optional second write-port for ToInt micro-ops.
class FPUExeUnit(
   has_fpu   : Boolean = true,
   has_fdiv  : Boolean = false,
   has_fpiu  : Boolean = false,
   has_fphfpu: Boolean = false
   )
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = 3,
      num_rf_write_ports = 2, // one for FRF, oen for IRF
      num_bypass_stages = 0,
      data_width = 65,
      bypassable = false,
      has_alu  = false,
      has_fpu  = has_fpu,
      has_fdiv = has_fdiv,
      has_fpiu = has_fpiu,
      has_fphfpu = has_fphfpu)(p)
{
   println ("     ExeUnit--")
   if (has_fpu) println ("       - FPU (Latency: " + dfmaLatency + ")")
   if (has_fdiv) println ("       - FDiv/FSqrt")
   if (has_fpiu) println ("       - FPIU (writes to Integer RF)")

   val fdiv_busy = Wire(init=Bool(false))
   val fpiu_busy = Wire(init=Bool(false))

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()

   io.fu_types := Mux(Bool(has_fpu), FU_FPU, Bits(0)) |
                  Mux(!fdiv_busy && Bool(has_fdiv), FU_FDV, Bits(0)) |
                  Mux(!fpiu_busy && Bool(has_fpiu), FU_F2I, Bits(0))


   io.resp(0).bits.writesToIRF = false
   io.resp(0).bits.writesToFRF = false
   io.resp(1).bits.writesToIRF = true  // always open ??  -- Jecy
   io.resp(1).bits.writesToFRF = false

   // FPU Unit -----------------------
   var fpu: FPUUnit = null
   val fpu_resp_val = Wire(init=Bool(false))
   val fpu_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fpu_resp_fflags.valid := Bool(false)
   if (has_fpu)
   {
      fpu = Module(new FPUUnit())
      fpu.io.req.valid           := io.req.valid &&
                                    (io.req.bits.uop.fu_code_is(FU_FPU) ||
                                    io.req.bits.uop.fu_code_is(FU_F2I)) // TODO move to using a separate unit
      fpu.io.req.bits.uop        := io.req.bits.uop
      fpu.io.req.bits.rs1_data   := io.req.bits.rs1_data
      fpu.io.req.bits.rs2_data   := io.req.bits.rs2_data
      fpu.io.req.bits.rs3_data   := io.req.bits.rs3_data
      fpu.io.req.bits.kill       := io.req.bits.kill
      fpu.io.fcsr_rm             := io.fcsr_rm
      fpu.io.brinfo <> io.brinfo
      fpu_resp_val := fpu.io.resp.valid
      fpu_resp_fflags := fpu.io.resp.bits.fflags
      fu_units += fpu
   }


   // FDiv/FSqrt Unit -----------------------
   var fdivsqrt: FDivSqrtUnit = null
   val fdiv_resp_val = Wire(init=Bool(false))
   val fdiv_resp_uop = Wire(new MicroOp())
   val fdiv_resp_data = Wire(Bits(width=65))
   val fdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fdiv_resp_fflags.valid := Bool(false)
   if (has_fdiv)
   {
      fdivsqrt = Module(new FDivSqrtUnit())
      fdivsqrt.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV)
      fdivsqrt.io.req.bits.uop      := io.req.bits.uop
      fdivsqrt.io.req.bits.rs1_data := io.req.bits.rs1_data
      fdivsqrt.io.req.bits.rs2_data := io.req.bits.rs2_data
      fdivsqrt.io.req.bits.kill     := io.req.bits.kill
      fdivsqrt.io.fcsr_rm           := io.fcsr_rm
      fdivsqrt.io.brinfo <> io.brinfo

      // share write port with the pipelined units
      fdivsqrt.io.resp.ready := !(fu_units.map(_.io.resp.valid).reduce(_|_)) // TODO PERF will get blocked by fpiu.

      fdiv_busy := !fdivsqrt.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV))

      fdiv_resp_val := fdivsqrt.io.resp.valid
      fdiv_resp_uop := fdivsqrt.io.resp.bits.uop
      fdiv_resp_data := fdivsqrt.io.resp.bits.data
      fdiv_resp_fflags := fdivsqrt.io.resp.bits.fflags

      fu_units += fdivsqrt
   }

   // Outputs (Write Port #0)  ---------------

   io.resp(0).valid    := fu_units.map(_.io.resp.valid).reduce(_|_) &&
                          !(fpu.io.resp.valid && fpu.io.resp.bits.uop.fu_code_is(FU_F2I))
   io.resp(0).bits.uop := new MicroOp().fromBits(
                           PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.uop.asUInt))))
   io.resp(0).bits.data:= PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt // Return the fisrt true valid. -- Jecy
   io.resp(0).bits.fflags := Mux(fpu_resp_val, fpu_resp_fflags, fdiv_resp_fflags)

   // Outputs (Write Port #1) -- FpToInt Queuing Unit -----------------------
   //io.resp(1).bits.writesToIRF = io.req.bits.uop.fu_code_is(FU_F2I) == true
   //if(io.req.bits.uop.fu_code_is(FU_F2I)==true){
   //   io.resp(1).bits.writesToIRF = true
   //}

   // TODO instantiate our own fpiu; and remove it from fpu.scala.

   // buffer up results since we share write-port on integer regfile. and HFP regfile
   val queue = Module(new QueueForMicroOpWithData(entries = dfmaLatency + 3, data_width)) // TODO being overly conservative
   queue.io.enq.valid       := fpu.io.resp.valid && fpu.io.resp.bits.uop.fu_code_is(FU_F2I)
   queue.io.enq.bits.uop    := fpu.io.resp.bits.uop
   queue.io.enq.bits.data   := fpu.io.resp.bits.data
   queue.io.enq.bits.fflags := fpu.io.resp.bits.fflags
   queue.io.brinfo          := io.brinfo
   queue.io.flush           := io.req.bits.kill
   io.resp(1) <> queue.io.deq
   //io.resp(1).bits.writesToIRF = queue.io.deq.bits.uop.fu_code == FU_F2I

   fpiu_busy := !(queue.io.empty)

   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.
}

// HFPU-only unit, with optional second write-port for ToInt micro-ops.
class HFPUExeUnit(
   has_hfpu  : Boolean = true,
   has_hfdiv : Boolean = false,
   has_hfpiu : Boolean = false,
   has_hfpfpu : Boolean = false
   )
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = 3,
      num_rf_write_ports = 2, // one for FRF, oen for IRF
      num_bypass_stages = 0,
      data_width = 65,
      bypassable = false,
      has_alu  = false,
      has_hfpu  = has_hfpu,
      has_hfdiv = has_hfdiv,
      has_hfpiu = has_hfpiu,
      has_hfpfpu = has_hfpfpu)(p)
{
   if(DEBUG_PRINTF_HFPU_PATH){
      printf("==========[Come into HFPUExeUnit]==========\n")
   }

   println ("     ExeUnit--")
   if (has_hfpu) println ("       - HFPU (Latency: " + hfmaLatency + ")")
   if (has_hfdiv) println ("       - HFDiv/FSqrt")
   if (has_hfpiu) println ("       - HFPIU (writes to Integer RF)")
   if (has_hfpfpu) println ("       - HFPFPU (write to FP RF)")

   val hfdiv_busy = Wire(init=Bool(false))
   val hfpiu_busy = Wire(init=Bool(false))
   val hfpfpu_busy= Wire(init=Bool(false))

   // The Functional Units --------------------
   val fu_units = ArrayBuffer[FunctionalUnit]()
   //val feedback = Wire(Bits(0,10))

   io.fu_types := Mux(Bool(has_hfpu), FU_HFPU, Bits(0)) |
                  Mux(!hfdiv_busy && Bool(has_hfdiv), FU_HFDV, Bits(0)) |
                  Mux(!hfpiu_busy && Bool(has_hfpiu), FU_HF2I, Bits(0)) |
                  Mux(!hfpfpu_busy && Bool(has_hfpfpu), FU_HF2F, Bits(0))

   if(DEBUG_PRINTF_HFPU){
       printf("HFPUExeUnit----------------------------------------------\n")
       printf("io.fu_types=[%x]\n",io.fu_types)
       printf("HFPUExeUnit----------------------------------------------\n")
   }


   io.resp(0).bits.writesToIRF = false
   io.resp(0).bits.writesToFRF = false
   //io.resp(1).bits.writesToIRF = true
   //io.resp(1).bits.writesToFRF = true

   // HFPU Unit -----------------------
   var hfpu: HFPUUnit = null
   val hfpu_resp_val = Wire(init=Bool(false))  // TODO: Where is the output?  -- Jecy
   val hfpu_resp_fflags = Wire(new ValidIO(new FFlagsResp()))  // TODO: Where is the output? -- Jecy
   hfpu_resp_fflags.valid := Bool(false)
   if (has_hfpu)
   {
      if(DEBUG_PRINTF_HFPU_PATH){
         printf("==========[New a HFPUUnit]==========\n")
      }

      hfpu = Module(new HFPUUnit())
      hfpu.io.req.valid           := io.req.valid &&
                                    (io.req.bits.uop.fu_code_is(FU_HFPU) ||
                                    io.req.bits.uop.fu_code_is(FU_HF2I)  || // TODO move to using a separate unit
                                    io.req.bits.uop.fu_code_is(FU_HF2F))    // TODO move to using a separete unit ?? -- Jecy
      hfpu.io.req.bits.uop        := io.req.bits.uop
      hfpu.io.req.bits.rs1_data   := io.req.bits.rs1_data
      hfpu.io.req.bits.rs2_data   := io.req.bits.rs2_data
      hfpu.io.req.bits.rs3_data   := io.req.bits.rs3_data
      hfpu.io.req.bits.kill       := io.req.bits.kill
      hfpu.io.fcsr_rm             := io.fcsr_rm
      hfpu.io.brinfo <> io.brinfo
      hfpu_resp_val := hfpu.io.resp.valid
      hfpu_resp_fflags := hfpu.io.resp.bits.fflags
      fu_units += hfpu

      if(DEBUG_PRINTF_HFPU){
         printf("HFPUExeUnit-Start--------------------------------------------------------------------------------------------\n")
         printf("io.req.rs1=[%x]    io.req.rs2=[%x]    io.req.rs3=[%x]\n",
                 io.req.bits.rs1_data,io.req.bits.rs2_data,io.req.bits.rs3_data);
         printf("hfpu.io.req.valid=[%d]    hfpu.io.req.rs1=[%x]    hfpu.io.req.rs2=[%x]    hfpu.io.req.rs3=[%x]\n",
                 hfpu.io.req.valid.asUInt,hfpu.io.req.bits.rs1_data,hfpu.io.req.bits.rs2_data,hfpu.io.req.bits.rs3_data);
         printf("hfpu.io.resp.valid=[%d]    hfpu.io.resp.bits.data=[%x]\n",hfpu.io.resp.valid.asUInt,hfpu.io.resp.bits.data)
         printf("HFPUExeUnit-End--------------------------------------------------------------------------------------------\n")
      }


   }


   // HFDiv/HFSqrt Unit -----------------------
   var hfdivsqrt: HFDivSqrtUnit = null
   val hfdiv_resp_val = Wire(init=Bool(false))
   val hfdiv_resp_uop = Wire(new MicroOp())
   val hfdiv_resp_data = Wire(Bits(width=65))
   val hfdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   hfdiv_resp_fflags.valid := Bool(false)
   if (has_hfdiv)
   {
      hfdivsqrt = Module(new HFDivSqrtUnit())
      hfdivsqrt.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_HFDV)
      hfdivsqrt.io.req.bits.uop      := io.req.bits.uop
      hfdivsqrt.io.req.bits.rs1_data := io.req.bits.rs1_data
      hfdivsqrt.io.req.bits.rs2_data := io.req.bits.rs2_data
      hfdivsqrt.io.req.bits.kill     := io.req.bits.kill
      hfdivsqrt.io.fcsr_rm           := io.fcsr_rm
      hfdivsqrt.io.brinfo <> io.brinfo

      // share write port with the pipelined units
      hfdivsqrt.io.resp.ready := !(fu_units.map(_.io.resp.valid).reduce(_|_)) // TODO PERF will get blocked by fpiu.

      hfdiv_busy := !hfdivsqrt.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code_is(FU_HFDV))

      hfdiv_resp_val := hfdivsqrt.io.resp.valid
      hfdiv_resp_uop := hfdivsqrt.io.resp.bits.uop
      hfdiv_resp_data := hfdivsqrt.io.resp.bits.data
      hfdiv_resp_fflags := hfdivsqrt.io.resp.bits.fflags

      fu_units += hfdivsqrt
   }

   // Outputs (Write Port #0)  ---------------

   io.resp(0).valid    := fu_units.map(_.io.resp.valid).reduce(_|_) &&
                          !((hfpu.io.resp.valid && hfpu.io.resp.bits.uop.fu_code_is(FU_HF2I)) ||
                            (hfpu.io.resp.valid && hfpu.io.resp.bits.uop.fu_code_is(FU_HF2F)))
   io.resp(0).bits.uop := new MicroOp().fromBits(
                           PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.uop.asUInt))))
   io.resp(0).bits.data:= PriorityMux(fu_units.map(f => (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   io.resp(0).bits.fflags := Mux(hfpu_resp_val, hfpu_resp_fflags, hfdiv_resp_fflags)

   if(DEBUG_PRINTF_HFPU){
      printf("HFPUExeUnit-Start--------------------------------------------------------------------------------------------\n")
      printf("hfpu_resp_val=[%d]    hfdiv_resp_val=[%d]    fu_units.map(_.io.resp.valid).reduce(_|_)=[%d]\n",
              hfpu_resp_val.asUInt, hfdiv_resp_val.asUInt, fu_units.map(_.io.resp.valid).reduce(_|_).asUInt)
      printf("io.resp[0].uop.uopc=[%d]    io.resp[0].uop.dst_rtype=[%d]    io.resp[0].data=[%x]    io.resp[0].valid=[%d]\n",
              io.resp(0).bits.uop.uopc,   io.resp(0).bits.uop.dst_rtype,   io.resp(0).bits.data,   io.resp(0).valid.asUInt);
      //printf("HFPUExeUnit-End--------------------------------------------------------------------------------------------\n")
   }
 

   // Outputs (Write Port #1) -- FpToInt Queuing Unit -----------------------
   //if(io.req.bits.uop.fu_code_is(FU_HF2I)==true){
   //   io.resp(1).bits.writesToFRF = false
   //} else if(io.req.bits.uop.fu_code_is(FU_HF2F)==true){
   //   io.resp(1).bits.writesToIRF = false
   //}else{
   //   io.resp(1).bits.writesToIRF = false
   //   io.resp(1).bits.writesToFRF = false
   //}

   //io.resp(1).bits.writesToIRF = io.req.bits.uop.fu_code_is(FU_HF2I)==true // Why error???
   //io.resp(1).bits.writesToFRF = io.req.bits.uop.fu_code_is(FU_HF2F)==true
   //io.resp(1).bits.writesToIRF = if(io.req.bits.uop.fu_code_is(FU_HF2I)==true) true else false
   //io.resp(1).bits.writesToFRF = if(io.req.bits.uop.fu_code_is(FU_HF2F)==true) true else false

   if(DEBUG_PRINTF_HFPU){
      printf("io.resp[0].bits.writesToFRF=[%d]    io.resp[0].bits.writesToIRF=[%d]    io.resp[1].bits.writesToFRF=[%d]    io.resp[1].bits.writesToIRF=[%d]\n",
              if(io.resp(0).bits.writesToFRF) UInt(1) else UInt(0),
              if(io.resp(0).bits.writesToIRF) UInt(1) else UInt(0),
              if(io.resp(1).bits.writesToFRF) UInt(1) else UInt(0),
              if(io.resp(1).bits.writesToIRF) UInt(1) else UInt(0))
      //printf("io.req.bits.uop.fu_code=[%d]    io.req.bits.uop.fu_code_is(FU_HF2I)=[%d]\n",
       //       io.req.bits.uop.fu_code,        io.req.bits.uop.fu_code_is(FU_HF2I))
   }
 
   // TODO instantiate our own fpiu; and remove it from hfpu.scala.

// Assumption: enq.valid only high if not killed by branch (so don't check IsKilled on io.enq).
class QueueForMicroOpWithDataLwl(entries: Int, data_width: Int)(implicit p: config.Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val feedback= Bool(INPUT)
      val enq     = Decoupled(new ExeUnitResp(data_width)).flip
      val deq     = Decoupled(new ExeUnitResp(data_width))

      val brinfo  = new BrResolutionInfo().asInput
      val flush   = Bool(INPUT)

      val empty   = Bool(OUTPUT)
      val count   = UInt(OUTPUT, log2Up(entries))
   })

   private val ram     = Mem(entries, new ExeUnitResp(data_width))
   private val valids  = Reg(init = Vec.fill(entries) {Bool(false)})
   private val brmasks = Reg(Vec(entries, UInt(width = MAX_BR_COUNT)))

   private val enq_ptr = Counter(entries)
   private val deq_ptr = Counter(entries)
   private val maybe_full = Reg(init=false.B)

   private val ptr_match = enq_ptr.value === deq_ptr.value
   io.empty := ptr_match && !maybe_full
   private val full = ptr_match && maybe_full
   private val do_enq = Wire(init=io.enq.fire())

   private val deq_ram_valid = Wire(init= !(io.empty))
   private val do_deq = Wire(init=io.deq.ready && deq_ram_valid && !io.feedback)

   for (i <- 0 until entries)
   {
      val mask = brmasks(i)
      valids(i)  := valids(i) && !IsKilledByBranch(io.brinfo, mask) && !io.flush
      when (valids(i)) {
         brmasks(i) := GetNewBrMask(io.brinfo, mask)
      }
   }

   ram(enq_ptr.value) := io.enq.bits
   valids(enq_ptr.value) := Mux(do_enq,true.B,false.B)//!IsKilledByBranch(io.brinfo, io.enq.bits.uop)
   brmasks(enq_ptr.value) := GetNewBrMask(io.brinfo, io.enq.bits.uop)
   //when (do_enq) {
   //   ram(enq_ptr.value) := io.enq.bits
   //   valids(enq_ptr.value) := true.B //!IsKilledByBranch(io.brinfo, io.enq.bits.uop)
   //   brmasks(enq_ptr.value) := GetNewBrMask(io.brinfo, io.enq.bits.uop)
   //   enq_ptr.inc()
   //} otherwise {
   //   ram(enq_ptr.value) := io.enq.bits
   //   valids(enq_ptr.value) := false.B //!IsKilledByBranch(io.brinfo, io.enq.bits.uop)
   //   brmasks(enq_ptr.value) := GetNewBrMask(io.brinfo, io.enq.bits.uop)
   //   //ram(enq_ptr.value).uop := UInt(0) // Tomuch substructions need to be constructed.  -- Jecy
   //   //ram(enq_ptr.value).data := UInt(0)
   //   //ram(enq_ptr.value).fflags.valid := false.B
   //   //ram(enq_ptr.value).fflags.bits.uop := UInt(0)
   //   //ram(enq_ptr.value).fflags.bits.flags := UInt(0)
   //   //valids(enq_ptr.value) := false.B
   //   //brmasks(enq_ptr.value) := UInt(0)
   //}
   when (do_enq) {
      enq_ptr.inc()
   }
   when (do_deq) {
      deq_ptr.inc()
   }
   when (do_enq != do_deq) {
      maybe_full := do_enq
   }

   io.enq.ready := !full

   private val out = ram(deq_ptr.value)
   io.deq.valid := deq_ram_valid && valids(deq_ptr.value) && !IsKilledByBranch(io.brinfo, out.uop)
   io.deq.bits := out
   io.deq.bits.uop.br_mask := GetNewBrMask(io.brinfo, brmasks(deq_ptr.value))

   if(DEBUG_PRINTF_HFPU){
      printf("In-Queue-------------------------------------------------------\n")
      printf("queue-io.feedback=[%x]    do_enq=[%d]    enq_ptr.value=[%d]    do_deq=[%d]    deq_ptr.value=[%d]\n",
              io.feedback,              do_enq.asUInt, enq_ptr.value,        do_deq.asUInt, deq_ptr.value)
      printf("In-Queue-------------------------------------------------------\n")
   }

   // For flow queue behavior.
   //when (io.empty)
   //{
   //   io.deq.valid := io.enq.valid //&& !IsKilledByBranch(io.brinfo, io.enq.bits.uop)
   //   io.deq.bits := io.enq.bits
   //   io.deq.bits.uop.br_mask := GetNewBrMask(io.brinfo, io.enq.bits.uop)

   //   do_deq := false.B
   //   when (io.deq.ready) { do_enq := false.B }
   //}

   private val ptr_diff = enq_ptr.value - deq_ptr.value
   if (isPow2(entries)) {
      io.count := Cat(maybe_full && ptr_match, ptr_diff)
   } else {
      io.count := Mux(ptr_match,
                     Mux(maybe_full,
                        entries.asUInt, 0.U),
                     Mux(deq_ptr.value > enq_ptr.value,
                        entries.asUInt + ptr_diff, ptr_diff))
   }
}

   // buffer up results since we share write-port on integer regfile. and HFP regfile
   val queue = Module(new QueueForMicroOpWithDataLwl(entries = dfmaLatency + 6, data_width)) // TODO being overly conservative
   //val wback_resp_valid      = Reg(Bits(0,10))
   //wbac_resp
   //val hfpu_io_resp_valid   := hfpu.io.resp.valid || (io.feedback =/= UInt(0))
   queue.io.feedback        := io.feedback =/= UInt(0)
   queue.io.enq.valid       := hfpu.io.resp.valid && (hfpu.io.resp.bits.uop.fu_code_is(FU_HF2I) ||
                                                      hfpu.io.resp.bits.uop.fu_code_is(FU_HF2F))
   queue.io.enq.bits.uop    := hfpu.io.resp.bits.uop
   queue.io.enq.bits.data   := hfpu.io.resp.bits.data
   queue.io.enq.bits.fflags := hfpu.io.resp.bits.fflags
   queue.io.brinfo          := io.brinfo
   queue.io.flush           := io.req.bits.kill
   io.resp(1) <> queue.io.deq
   //io.resp(1).bits.writesToIRF = queue.io.deq.bits.uop.fu_code == FU_HF2I
   //io.resp(1).bits.writesToFRF = queue.io.deq.bits.uop.fu_code == FU_HF2F

   // Enter queue only once per cycle -- Jecy
   //queue.io.enq.valid       := io.feedback =/= UInt(0)
   //queue.io.enq.bits.uop    := io.resp(1).bits.uop
   //queue.io.enq.bits.data   := io.resp(1).bits.data
   //queue.io.enq.bits.fflags := io.resp(1).bits.fflags
   //queue.io.brinfo          := io.resp(1).brinfo
   //queue.io.flush           := io.resp(1).flush


   if(DEBUG_PRINTF_HFPU){
      printf("HFPUExeUnit-Start--------------------------------------------------------------------------------------------\n")
      printf("io.feedback=[%x]\n",io.feedback)
      printf("io.resp[1].uop.uopc=[%d]    io.resp[1].uop.dst_rtype=[%d]    io.resp[1].data=[%x]    io.resp[1].valid=[%d]\n",
              io.resp(1).bits.uop.uopc,   io.resp(1).bits.uop.dst_rtype,   io.resp(1).bits.data,   io.resp(1).valid.asUInt);
      printf("HFPUExeUnit-End--------------------------------------------------------------------------------------------\n")
   }
 
   hfpiu_busy := !(queue.io.empty)
   hfpfpu_busy := !(queue.io.empty)

   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.
}



class FDivSqrtExeUnit(implicit p: Parameters)
   extends ExecutionUnit(num_rf_read_ports = 2
                                       , num_rf_write_ports = 1
                                       , num_bypass_stages = 0
                                       , data_width = 65
                                       , num_variable_write_ports = 1
                                       , has_fdiv = true
                                       )
{
   println ("     ExeUnit--")
   println ("       - FDiv/FSqrt")
   val fdiv_busy = Wire(Bool())
   io.fu_types := Mux(!fdiv_busy, FU_FDV, Bits(0))

   val fdivsqrt = Module(new FDivSqrtUnit())
   fdivsqrt.io.req <> io.req
   fdivsqrt.io.fcsr_rm    := io.fcsr_rm
   io.resp(0).valid       := fdivsqrt.io.resp.valid
   io.resp(0).bits.uop    := fdivsqrt.io.resp.bits.uop
   io.resp(0).bits.data   := fdivsqrt.io.resp.bits.data
   io.resp(0).bits.fflags := fdivsqrt.io.resp.bits.fflags
   fdivsqrt.io.brinfo <> io.brinfo
   io.bypass <> fdivsqrt.io.bypass

   fdiv_busy := !fdivsqrt.io.req.ready || io.req.valid
}


class IntToFPExeUnit(implicit p: Parameters) extends ExecutionUnit(
   has_ifpu = true,
   num_rf_read_ports = 2,
   num_rf_write_ports = 1,
   num_bypass_stages = 0,
   data_width = 65,
   // don't schedule uops from issue-window -- we're hard-hacking the datapath,
   // since the operand data comes from the IRF but writes back to the FRF.
   uses_iss_unit = false)
{
   println ("     ExeUnit--")
   println ("       - IntToFP")
   val busy = Wire(init=Bool(false))
   io.fu_types := Mux(!busy, FU_I2F, Bits(0))
   io.resp(0).bits.writesToIRF = false
   io.resp(0).bits.writesToFRF = false

   val ifpu = Module(new IntToFPUnit())
   ifpu.io.req <> io.req
   ifpu.io.fcsr_rm := io.fcsr_rm
   ifpu.io.brinfo <> io.brinfo
   io.bypass <> ifpu.io.bypass

   // buffer up results since we share write-port on integer regfile.
   val queue = Module(new QueueForMicroOpWithData(entries = p(BoomKey).intToFpLatency + 3, data_width)) // TODO being overly conservative
   queue.io.enq.valid       := ifpu.io.resp.valid
   queue.io.enq.bits.uop    := ifpu.io.resp.bits.uop
   queue.io.enq.bits.data   := ifpu.io.resp.bits.data
   queue.io.enq.bits.fflags := ifpu.io.resp.bits.fflags
   queue.io.brinfo := io.brinfo
   queue.io.flush := io.req.bits.kill

   io.resp(0) <> queue.io.deq

   busy := !(queue.io.empty)


   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.
}

class FPToHFPExeUnit(implicit p: Parameters) extends ExecutionUnit(
   has_fphfpu = true,
   num_rf_read_ports = 2,
   num_rf_write_ports = 1,
   num_bypass_stages = 0,
   data_width = 65,
   // don't schedule uops from issue-window -- we're hard-hacking the datapath,
   // since the operand data comes from the IRF but writes back to the FRF.
   uses_iss_unit = false)
{
   println ("     ExeUnit--")
   println ("       - FPToHFP")
   val busy = Wire(init=Bool(false))
   io.fu_types := Mux(!busy, FU_F2HF, Bits(0))
   io.resp(0).bits.writesToIRF = false
   io.resp(0).bits.writesToFRF = false

   val fphfpu = Module(new FPToHFPUnit())
   fphfpu.io.req <> io.req
   fphfpu.io.fcsr_rm := io.fcsr_rm
   fphfpu.io.brinfo <> io.brinfo
   io.bypass <> fphfpu.io.bypass

   // buffer up results since we share write-port on integer regfile.
   val queue = Module(new QueueForMicroOpWithData(entries = p(BoomKey).fpToHfpLatency + 3, data_width)) // TODO being overly conservative
   queue.io.enq.valid       := fphfpu.io.resp.valid
   queue.io.enq.bits.uop    := fphfpu.io.resp.bits.uop
   queue.io.enq.bits.data   := fphfpu.io.resp.bits.data
   queue.io.enq.bits.fflags := fphfpu.io.resp.bits.fflags
   queue.io.brinfo := io.brinfo
   queue.io.flush := io.req.bits.kill

   io.resp(0) <> queue.io.deq

   busy := !(queue.io.empty)


   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.
}



class IntToHFPExeUnit(implicit p: Parameters) extends ExecutionUnit(
   has_ihfpu = true,
   num_rf_read_ports = 2,
   num_rf_write_ports = 1,
   num_bypass_stages = 0,
   data_width = 65,
   // don't schedule uops from issue-window -- we're hard-hacking the datapath,
   // since the operand data comes from the IRF but writes back to the HFRF.
   uses_iss_unit = false)
{
   println ("     ExeUnit--")
   println ("       - IntToHFP")
   val busy = Wire(init=Bool(false))
   io.fu_types := Mux(!busy, FU_I2HF, Bits(0))
   io.resp(0).bits.writesToIRF = false
   io.resp(0).bits.writesToFRF = false

   val ihfpu = Module(new IntToHFPUnit())
   ihfpu.io.req <> io.req
   ihfpu.io.fcsr_rm := io.fcsr_rm
   ihfpu.io.brinfo <> io.brinfo
   io.bypass <> ihfpu.io.bypass

   // buffer up results since we share write-port on integer regfile.
   val queue = Module(new QueueForMicroOpWithData(entries = p(BoomKey).intToHfpLatency + 3, data_width)) // TODO being overly conservative
   queue.io.enq.valid       := ihfpu.io.resp.valid
   queue.io.enq.bits.uop    := ihfpu.io.resp.bits.uop
   queue.io.enq.bits.data   := ihfpu.io.resp.bits.data
   queue.io.enq.bits.fflags := ihfpu.io.resp.bits.fflags
   queue.io.brinfo := io.brinfo
   queue.io.flush := io.req.bits.kill

   io.resp(0) <> queue.io.deq

   busy := !(queue.io.empty)


   assert (queue.io.enq.ready) // If this backs up, we've miscalculated the size of the queue.
}



class MemExeUnit(implicit p: Parameters) extends ExecutionUnit(num_rf_read_ports = 2,
   num_rf_write_ports = 1,
   num_bypass_stages = 0,
   data_width = if(p(tile.TileKey).core.fpu.nonEmpty) 65 else p(tile.XLen),
   num_variable_write_ports = 1,
   bypassable = false,
   is_mem_unit = true)(p)
{
   println ("     ExeUnit--")
   println ("       - Mem")

   io.fu_types := FU_MEM

   // Perform address calculation
   val maddrcalc = Module(new MemAddrCalcUnit())
   maddrcalc.io.req <> io.req

   maddrcalc.io.brinfo <> io.brinfo
   io.bypass <> maddrcalc.io.bypass  // TODO this is not where the bypassing should occur from, is there any bypassing happening?!

   // enqueue addresses,st-data at the end of Execute
   io.lsu_io.exe_resp <> maddrcalc.io.resp


   // TODO get rid of com_exception and guard with an assert? Need to surpress within dc-shim.
//   assert (!(io.com_exception && lsu.io.memreq_uop.is_load && lsu.io.memreq_val),
//      "[execute] a valid load is returning while an exception is being thrown.")
   io.dmem.req.valid      := Mux(io.com_exception && io.lsu_io.memreq_uop.is_load,
                              Bool(false),
                              io.lsu_io.memreq_val)
   io.dmem.req.bits.addr  := io.lsu_io.memreq_addr
   io.dmem.req.bits.data  := io.lsu_io.memreq_wdata
   io.dmem.req.bits.uop   := io.lsu_io.memreq_uop
   io.dmem.req.bits.kill  := io.lsu_io.memreq_kill // load kill request sent to memory

   // I should be timing forwarding to coincide with dmem resps, so I'm not clobbering
   //anything....
   val memresp_val    = Mux(io.com_exception && io.dmem.resp.bits.uop.is_load, Bool(false),
                                                io.lsu_io.forward_val || io.dmem.resp.valid)
   val memresp_rf_wen = (io.dmem.resp.valid && (io.dmem.resp.bits.uop.mem_cmd === M_XRD || io.dmem.resp.bits.uop.is_amo)) ||  // TODO should I refactor this to use is_load?
                           io.lsu_io.forward_val
   val memresp_uop    = Mux(io.lsu_io.forward_val, io.lsu_io.forward_uop,
                                                io.dmem.resp.bits.uop)

   val memresp_data = Mux(io.lsu_io.forward_val, io.lsu_io.forward_data, io.dmem.resp.bits.data_subword)

   io.lsu_io.memresp.valid := memresp_val
   io.lsu_io.memresp.bits  := memresp_uop


   // Hook up loads to the response
   io.resp(0).valid := memresp_val
   io.resp(0).bits.uop := memresp_uop
   io.resp(0).bits.uop.ctrl.rf_wen := memresp_rf_wen
   io.resp(0).bits.data := memresp_data
}


class ALUMemExeUnit(
   is_branch_unit    : Boolean = false,
   shares_csr_wport: Boolean = false,
   fp_mem_support  : Boolean = true, // does memory need to support FP loads/stores?
   has_fpu         : Boolean = false,
   has_mul         : Boolean = false,
   has_div         : Boolean = false,
   has_fdiv        : Boolean = false,
   use_slow_mul    : Boolean = false)
   (implicit p: Parameters)
   extends ExecutionUnit(
      num_rf_read_ports = if (has_fpu) 3 else 2,
      num_rf_write_ports = 2,
      num_bypass_stages = if (has_fpu) p(tile.TileKey).core.fpu.get.dfmaLatency else if (has_mul && !use_slow_mul) 3 else 1,
      data_width = if (fp_mem_support) 65 else 64,
      num_variable_write_ports = 1,
      bypassable = true,
      is_mem_unit = true,
      uses_csr_wport = shares_csr_wport,
      is_branch_unit = is_branch_unit,
      has_alu = true,
      has_fpu = has_fpu,
      has_mul = has_mul,
      has_div = has_div,
      has_fdiv = has_fdiv)(p)
{
   println ("     ExeUnit--")
   println ("       - ALU")
   if (has_fpu) println ("       - FPU (Latency: " + dfmaLatency + " cycles)")
   if (has_mul && !use_slow_mul) println ("       - Mul (pipelined: " + imulLatency + " cycles)")
   if (has_div && has_mul && use_slow_mul) println ("       - Mul/Div (unpipelined)")
   else if (has_mul && use_slow_mul) println ("       - Mul (unpipelined)")
   else if (has_div) println ("       - Div")
   if (has_fdiv) println ("       - FDiv/FSqrt")
   println ("       - Mem")

   val muldiv_busy = Wire(Bool())
   val fdiv_busy = Wire(Bool())
   io.fu_types := FU_ALU |
                  FU_MEM |
                  Mux(Bool(has_fpu), FU_FPU, Bits(0)) |
                  (Mux(Bool(has_mul && !use_slow_mul), FU_MUL, Bits(0))) |
                  (Mux(!muldiv_busy && Bool(use_slow_mul), FU_MUL, Bits(0))) |
                  (Mux(!muldiv_busy && Bool(has_div), FU_DIV, Bits(0))) |
                  (Mux(Bool(shares_csr_wport), FU_CSR, Bits(0))) |
                  Mux(Bool(is_branch_unit), FU_BRU, Bits(0)) |
                  Mux(!fdiv_busy && Bool(has_fdiv), FU_FDV, Bits(0))


   val memresp_val = Wire(Bool())
   val fdiv_resp_val = Wire(Bool())


   // ALU Unit -------------------------------
   val alu = Module(new ALUUnit(is_branch_unit = is_branch_unit, num_stages = num_bypass_stages))
   alu.io.req.valid         := io.req.valid &&
                                    (io.req.bits.uop.fu_code_is(FU_ALU) ||
                                     io.req.bits.uop.fu_code_is(FU_BRU) ||
                                     io.req.bits.uop.fu_code_is(FU_CSR))
   alu.io.req.bits.uop      := io.req.bits.uop
   alu.io.req.bits.kill     := io.req.bits.kill
   alu.io.req.bits.rs1_data := io.req.bits.rs1_data
   alu.io.req.bits.rs2_data := io.req.bits.rs2_data

   alu.io.brinfo <> io.brinfo

   // branch unit is embedded inside the ALU
   if (is_branch_unit)
   {
      io.br_unit <> alu.io.br_unit
      alu.io.get_rob_pc <> io.get_rob_pc
      io.get_pred <> alu.io.get_pred
      alu.io.status <> io.status
   }
   else
   {
      io.br_unit.brinfo.valid := Bool(false)
   }

   // Pipelined, IMul Unit -----------------------
   var imul: PipelinedMulUnit = null
   if (!use_slow_mul)
   {
      imul = Module(new PipelinedMulUnit(imulLatency))
      imul.io.req.valid := io.req.valid && (io.req.bits.uop.fu_code_is(FU_MUL) && Bool(!use_slow_mul))
      imul.io.req.bits.uop      := io.req.bits.uop
      imul.io.req.bits.rs1_data := io.req.bits.rs1_data
      imul.io.req.bits.rs2_data := io.req.bits.rs2_data
      imul.io.req.bits.kill     := io.req.bits.kill
      imul.io.brinfo <> io.brinfo
      if (has_fpu) require (imulLatency == dfmaLatency)
   }

   // FPU Unit -----------------------
   var fpu: FPUUnit = null
   if (has_fpu)
   {
      fpu = Module(new FPUUnit())
      fpu.io.req.valid           := io.req.valid && io.req.bits.uop.fu_code_is(FU_FPU)
      fpu.io.req.bits.uop        := io.req.bits.uop
      fpu.io.req.bits.rs1_data   := io.req.bits.rs1_data
      fpu.io.req.bits.rs2_data   := io.req.bits.rs2_data
      fpu.io.req.bits.rs3_data   := io.req.bits.rs3_data
      fpu.io.req.bits.kill       := io.req.bits.kill
      fpu.io.fcsr_rm             := io.fcsr_rm
      fpu.io.brinfo <> io.brinfo
      // TODO use bundle interfacing
   }

   // Outputs (Write Port #0)  ---------------

   val fu_units = ArrayBuffer[FunctionalUnit]()
   fu_units += alu
   if (has_mul && !use_slow_mul) fu_units += imul
   if (has_fpu) fu_units += fpu

   io.resp(0).valid    := fu_units.map(_.io.resp.valid).reduce(_|_)
   io.resp(0).bits.uop := new MicroOp().fromBits(PriorityMux(fu_units.map(f =>
      (f.io.resp.valid, f.io.resp.bits.uop.asUInt))))
   io.resp(0).bits.data:= PriorityMux(fu_units.map(f =>
      (f.io.resp.valid, f.io.resp.bits.data.asUInt))).asUInt
   // pulled out for critical path reasons
   io.resp(0).bits.uop.csr_addr := ImmGen(alu.io.resp.bits.uop.imm_packed, IS_I).asUInt
   io.resp(0).bits.uop.ctrl.csr_cmd := alu.io.resp.bits.uop.ctrl.csr_cmd

   if (has_fpu)
   {
      io.resp(0).bits.fflags.valid      := fpu.io.resp.valid
      io.resp(0).bits.fflags.bits.uop   := fpu.io.resp.bits.fflags.bits.uop
      io.resp(0).bits.fflags.bits.flags := fpu.io.resp.bits.fflags.bits.flags
   }

   assert (PopCount(fu_units.map(_.io.resp.valid)) <= 1.U
      , "Multiple functional units are fighting over the write port.")

   // Mul/Div/Rem Unit -----------------------
   var muldiv: MulDivUnit = null
   val muldiv_resp_val = Wire(Bool())
   val muldiv_resp_uop = Wire(new MicroOp())
   val muldiv_resp_data = Wire(Bits(width=64))
   muldiv_resp_val := Bool(false)
   muldiv_busy := Bool(false)
   if (has_div || (has_mul && use_slow_mul))
   {
      muldiv = Module(new MulDivUnit())

      muldiv.io.req.valid           := io.req.valid &&
                                       ((io.req.bits.uop.fu_code_is(FU_DIV) && Bool(has_div)) ||
                                       (io.req.bits.uop.fu_code_is(FU_MUL) && Bool(has_mul && use_slow_mul)))
      muldiv.io.req.bits.uop        := io.req.bits.uop
      muldiv.io.req.bits.rs1_data   := io.req.bits.rs1_data
      muldiv.io.req.bits.rs2_data   := io.req.bits.rs2_data
      muldiv.io.req.bits.kill       := io.req.bits.kill

      muldiv.io.brinfo <> io.brinfo

      muldiv.io.resp.ready := !memresp_val && !fdiv_resp_val //share write port with the memory, fdiv

      muldiv_resp_val := muldiv.io.resp.valid
      muldiv_resp_uop := muldiv.io.resp.bits.uop
      muldiv_resp_data:= muldiv.io.resp.bits.data
      muldiv_busy := !muldiv.io.req.ready ||
                     (io.req.valid && (io.req.bits.uop.fu_code_is(FU_DIV) ||
                                      (io.req.bits.uop.fu_code_is(FU_MUL) && Bool(has_mul && use_slow_mul))))
   }


   // FDiv/FSqrt Unit -----------------------
   var fdivsqrt: FDivSqrtUnit = null
   val fdiv_resp_uop = Wire(new MicroOp())
   val fdiv_resp_data = Wire(Bits(width=65))
   val fdiv_resp_fflags = Wire(new ValidIO(new FFlagsResp()))
   fdiv_resp_val := Bool(false)
   fdiv_resp_fflags.valid := Bool(false)
   fdiv_busy := Bool(false)
   if (has_fdiv)
   {
      fdivsqrt = Module(new FDivSqrtUnit())
      fdivsqrt.io.req.valid         := io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV)
      fdivsqrt.io.req.bits.uop      := io.req.bits.uop
      fdivsqrt.io.req.bits.rs1_data := io.req.bits.rs1_data
      fdivsqrt.io.req.bits.rs2_data := io.req.bits.rs2_data
      fdivsqrt.io.req.bits.kill     := io.req.bits.kill
      fdivsqrt.io.fcsr_rm           := io.fcsr_rm
      fdivsqrt.io.brinfo <> io.brinfo

      fdivsqrt.io.resp.ready := !memresp_val //share write port with memory

      fdiv_busy := !fdivsqrt.io.req.ready || (io.req.valid && io.req.bits.uop.fu_code_is(FU_FDV))

      fdiv_resp_val := fdivsqrt.io.resp.valid
      fdiv_resp_uop := fdivsqrt.io.resp.bits.uop
      fdiv_resp_data := fdivsqrt.io.resp.bits.data
      fdiv_resp_fflags := fdivsqrt.io.resp.bits.fflags
   }

   // Bypassing --------------------------------
   // (only the ALU is bypassable)

   io.bypass <> alu.io.bypass

   // Perform address calculation
   val maddrcalc = Module(new MemAddrCalcUnit())
   maddrcalc.io.req <> io.req
   maddrcalc.io.brinfo <> io.brinfo

   // enqueue addresses,st-data at the end of Execute
   io.lsu_io.exe_resp <> maddrcalc.io.resp

   io.dmem.req.valid     := Mux(io.com_exception && io.lsu_io.memreq_uop.is_load, Bool(false),
                                                                              io.lsu_io.memreq_val)
   io.dmem.req.bits.addr  := io.lsu_io.memreq_addr
   io.dmem.req.bits.data  := io.lsu_io.memreq_wdata
   io.dmem.req.bits.uop   := io.lsu_io.memreq_uop
   io.dmem.req.bits.kill  := io.lsu_io.memreq_kill // load kill request sent to memory

   // I'm timing forwarding to coincide with dmem resps, so I'm not clobbering anything...
   memresp_val := Mux(io.com_exception && io.dmem.resp.bits.uop.is_load, Bool(false),
                                               io.lsu_io.forward_val || io.dmem.resp.valid)


   val memresp_rf_wen = (io.dmem.resp.valid && (io.dmem.resp.bits.uop.mem_cmd === M_XRD || io.dmem.resp.bits.uop.is_amo)) ||
                           io.lsu_io.forward_val
   val memresp_uop    = Mux(io.lsu_io.forward_val, io.lsu_io.forward_uop,
                                                io.dmem.resp.bits.uop)

   val memresp_data = Mux(io.lsu_io.forward_val, io.lsu_io.forward_data, io.dmem.resp.bits.data_subword)

   io.lsu_io.memresp.valid := memresp_val
   io.lsu_io.memresp.bits  := memresp_uop

   io.resp(1).valid                := memresp_val || fdiv_resp_val || muldiv_resp_val
   io.resp(1).bits.uop             := MuxCase(memresp_uop, Seq(
                                       memresp_val -> memresp_uop,
                                       fdiv_resp_val -> fdiv_resp_uop,
                                       muldiv_resp_val -> muldiv_resp_uop))
   io.resp(1).bits.uop.ctrl.rf_wen := (memresp_val && memresp_rf_wen) || fdiv_resp_val || muldiv_resp_val
   io.resp(1).bits.data            := MuxCase(memresp_data, Seq(
                                       memresp_val -> memresp_data,
                                       fdiv_resp_val -> fdiv_resp_data,
                                       muldiv_resp_val -> muldiv_resp_data))
   io.resp(1).bits.fflags          := fdiv_resp_fflags
}

