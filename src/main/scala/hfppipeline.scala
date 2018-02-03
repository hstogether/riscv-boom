//******************************************************************************
// Copyright (c) 2015, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Floating Point Datapath Pipeline
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Christopher Celio

// The floating point issue window, regfile, and arithmetic units are all handled here.


package boom

import Chisel._
import config.Parameters

import boom.FUConstants._


class HfpPipeline(implicit p: Parameters) extends BoomModule()(p)
{
   if(DEBUG_PRINTF_HFPU_PATH){
      printf("==========[Come in to HfpPipeline]==========\n")
   }

   val fpIssueParams = issueParams.find(_.iqType == IQT_HFP.litValue).get
   val num_ll_ports = 1 // hard-wired; used by mem port and i2f port.
   val num_wakeup_ports = fpIssueParams.issueWidth + num_ll_ports
   val fp_preg_sz = log2Up(numHfpPhysRegs)

   val io = new Bundle
   {
      val brinfo           = new BrResolutionInfo().asInput
      val flush_pipeline   = Bool(INPUT)
      val fcsr_rm          = UInt(INPUT, tile.FPConstants.RM_SZ)

      val dis_valids       = Vec(DISPATCH_WIDTH, Bool()).asInput // REFACTOR into single Decoupled()
      val dis_uops         = Vec(DISPATCH_WIDTH, new MicroOp()).asInput
      val dis_readys       = Vec(DISPATCH_WIDTH, Bool()).asOutput

      // +1 for recoding.
      val ll_wport         = Valid(new ExeUnitResp(fLen+1)).flip // from memory unit
      val fromint          = Valid(new FuncUnitReq(fLen+1)).flip // from integer RF
      val fromfp           = Valid(new FuncUnitReq(fLen+1)).flip // from FP RF
      val tosdq            = Valid(new MicroOpWithData(fLen))    // to Load/Store Unit
      val toint            = Decoupled(new ExeUnitResp(xLen))    // to integer RF
      val tofp             = Decoupled(new ExeUnitResp(fLen+1))  // to FP RF

      val wakeups          = Vec(num_wakeup_ports, Valid(new ExeUnitResp(fLen+1)))
      val wb_valids        = Vec(num_wakeup_ports, Bool()).asInput
      val wb_pdsts         = Vec(num_wakeup_ports, UInt(width=fp_preg_sz)).asInput

      //TODO -- hook up commit log stuff.
      val debug_tsc_reg    = UInt(INPUT, xLen)
   }

   //**********************************
   // construct all of the modules

   if(DEBUG_PRINTF_HFPU_PATH){
      printf("==========[New a boom.ExecutionUnits(hfpt=true)]==========\n")
   }

   val exe_units        = new boom.ExecutionUnits(hfpu=true)
   val issue_unit       = Module(new IssueUnitCollasping(
                           issueParams.find(_.iqType == IQT_HFP.litValue).get,
                           num_wakeup_ports))
   val fregfile         = Module(new RegisterFileBehavorial(numFpPhysRegs,
                                 exe_units.withFilter(_.uses_iss_unit).map(e => e.num_rf_read_ports).sum,
                                 // TODO get rid of -1, as that's a write-port going to IRF
                                 exe_units.withFilter(_.uses_iss_unit).map(e => e.num_rf_write_ports).sum - 1 +
                                    num_ll_ports,
                                 fLen+1,
                                 exe_units.bypassable_write_port_mask
                                 ))
   val fregister_read   = Module(new RegisterRead(
                           issue_unit.issue_width,
                           exe_units.withFilter(_.uses_iss_unit).map(_.supportedFuncUnits),
                           exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports).sum,
                           exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports),
                           exe_units.num_total_bypass_ports,
                           fLen+1))

   require (exe_units.withFilter(_.uses_iss_unit).map(x=>x).length == issue_unit.issue_width)

   // We're playing fast and loose on the number of wakeup and write ports.
   // The -1 is for the HF2I port; -1 for I2HF port; -1 for F2HF port.
   println (exe_units.map(_.num_rf_write_ports).sum)
   require (exe_units.map(_.num_rf_write_ports).sum-1-1-1 + num_ll_ports == num_wakeup_ports)
   require (exe_units.withFilter(_.uses_iss_unit).map(e =>
      e.num_rf_write_ports).sum -1 + num_ll_ports == num_wakeup_ports)

   override def toString: String =
      fregfile.toString +
      "\n   Num Wakeup Ports      : " + num_wakeup_ports +
      "\n   Num Bypass Ports      : " + exe_units.num_total_bypass_ports + "\n"


   //*************************************************************
   // Issue window logic

   val iss_valids     = Wire(Vec(exe_units.withFilter(_.uses_iss_unit).map(x=>x).length, Bool()))
   val iss_uops       = Wire(Vec(exe_units.withFilter(_.uses_iss_unit).map(x=>x).length, new MicroOp()))

   issue_unit.io.tsc_reg := io.debug_tsc_reg
   issue_unit.io.brinfo := io.brinfo
   issue_unit.io.flush_pipeline := io.flush_pipeline

   require (exe_units.num_total_bypass_ports == 0)

   //-------------------------------------------------------------
   // **** Dispatch Stage ****
   //-------------------------------------------------------------

   if(DEBUG_PRINTF_HFPU){
      printf("HfpPipeline--IO---------------------------------------------\n")
      for(w <- 0 until DISPATCH_WIDTH){
         printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    dis.valid=[%d]\n",
                 io.dis_uops(w).uopc,io.dis_uops(w).inst,
                 io.dis_uops(w).hfp_val.asUInt, io.dis_uops(w).fu_code,
                 io.dis_uops(w).iqtype, io.dis_uops(w).valid.asUInt,io.dis_valids.asUInt)
         printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]\n",
                 io.dis_uops(w).lrs1_rtype,io.dis_uops(w).lrs1,io.dis_uops(w).pop1, 
                 io.dis_uops(w).lrs2_rtype,io.dis_uops(w).lrs2,io.dis_uops(w).pop2,
                 io.dis_uops(w).lrs3_rtype,io.dis_uops(w).lrs3,io.dis_uops(w).pop3,
                 io.dis_uops(w).dst_rtype,io.dis_uops(w).ldst,io.dis_uops(w).pdst,io.dis_uops(w).ldst_val.asUInt)
      }
      printf("HfpPipeline--IO---------------------------------------------\n")
   }

   // Input (Dispatch)
   for (w <- 0 until DISPATCH_WIDTH)
   {
      issue_unit.io.dis_valids(w) := io.dis_valids(w) && io.dis_uops(w).iqtype === UInt(issue_unit.iqType)
      issue_unit.io.dis_uops(w) := io.dis_uops(w)

      // Or... add STDataGen micro-op for FP stores.
      when (io.dis_uops(w).uopc === uopSTA && io.dis_uops(w).lrs2_rtype === RT_FHT) {
         issue_unit.io.dis_valids(w) := io.dis_valids(w)
         issue_unit.io.dis_uops(w).uopc := uopSTD
         issue_unit.io.dis_uops(w).fu_code := FUConstants.FU_HFPU
         issue_unit.io.dis_uops(w).lrs1_rtype := RT_X
         issue_unit.io.dis_uops(w).prs1_busy := Bool(false)
      }
   }
   io.dis_readys := issue_unit.io.dis_readys

   if(DEBUG_PRINTF_HFPU){
      printf("HfpPipeline--Dispatch---------------------------------------------\n")
      for(w <- 0 until DISPATCH_WIDTH){
         printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    dis.valid=[%d]\n",
                 issue_unit.io.dis_uops(w).uopc,issue_unit.io.dis_uops(w).inst,
                 issue_unit.io.dis_uops(w).hfp_val.asUInt, issue_unit.io.dis_uops(w).fu_code,
                 issue_unit.io.dis_uops(w).iqtype, issue_unit.io.dis_uops(w).valid.asUInt,issue_unit.io.dis_valids.asUInt)
         printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]\n",
                 issue_unit.io.dis_uops(w).lrs1_rtype,issue_unit.io.dis_uops(w).lrs1,issue_unit.io.dis_uops(w).pop1, 
                 issue_unit.io.dis_uops(w).lrs2_rtype,issue_unit.io.dis_uops(w).lrs2,issue_unit.io.dis_uops(w).pop2,
                 issue_unit.io.dis_uops(w).lrs3_rtype,issue_unit.io.dis_uops(w).lrs3,issue_unit.io.dis_uops(w).pop3,
                 issue_unit.io.dis_uops(w).dst_rtype,issue_unit.io.dis_uops(w).ldst,issue_unit.io.dis_uops(w).pdst,issue_unit.io.dis_uops(w).ldst_val.asUInt)
      }
      printf("HfpPipeline--Dispatch---------------------------------------------\n")
   }


   //-------------------------------------------------------------
   // **** Issue Stage ****
   //-------------------------------------------------------------

   // Output (Issue)
   for (i <- 0 until issue_unit.issue_width)
   {
      iss_valids(i) := issue_unit.io.iss_valids(i)
      iss_uops(i) := issue_unit.io.iss_uops(i)

      var fu_types = exe_units(i).io.fu_types
      if (exe_units(i).supportedFuncUnits.fdiv && regreadLatency > 0)
      {
         val fdiv_issued = iss_valids(i) && iss_uops(i).fu_code_is(FU_HFDV)
         fu_types = fu_types & RegNext(~Mux(fdiv_issued, FU_HFDV, Bits(0)))
      }
      issue_unit.io.fu_types(i) := fu_types

      require (exe_units(i).uses_iss_unit)
      if(DEBUG_PRINTF_HFPU){
         printf("HfpPipeline--Issue Stage---------------------------------------------\n")
         printf("    Iss_uops\n")
         printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    dis.valid=[%d]\n",
                 iss_uops(i).uopc,iss_uops(i).inst,
                 iss_uops(i).hfp_val.asUInt,iss_uops(i).fu_code,
                 iss_uops(i).iqtype,iss_uops(i).valid.asUInt,iss_valids(i).asUInt)
         printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]\n",
                 iss_uops(i).lrs1_rtype,iss_uops(i).lrs1,iss_uops(i).pop1, 
                 iss_uops(i).lrs2_rtype,iss_uops(i).lrs2,iss_uops(i).pop2,
                 iss_uops(i).lrs3_rtype,iss_uops(i).lrs3,iss_uops(i).pop3,
                 iss_uops(i).dst_rtype, iss_uops(i).ldst,iss_uops(i).pdst,iss_uops(i).ldst_val.asUInt)

         printf("    Iss_unit\n")
         printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    dis.valid=[%d]\n",
                 issue_unit.io.iss_uops(i).uopc,issue_unit.io.iss_uops(i).inst,
                 issue_unit.io.iss_uops(i).hfp_val.asUInt,issue_unit.io.iss_uops(i).fu_code,
                 issue_unit.io.iss_uops(i).iqtype,issue_unit.io.iss_uops(i).valid.asUInt,issue_unit.io.iss_valids(i).asUInt)
         printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]\n",
                 issue_unit.io.iss_uops(i).lrs1_rtype,issue_unit.io.iss_uops(i).lrs1,issue_unit.io.iss_uops(i).pop1, 
                 issue_unit.io.iss_uops(i).lrs2_rtype,issue_unit.io.iss_uops(i).lrs2,issue_unit.io.iss_uops(i).pop2,
                 issue_unit.io.iss_uops(i).lrs3_rtype,issue_unit.io.iss_uops(i).lrs3,issue_unit.io.iss_uops(i).pop3,
                 issue_unit.io.iss_uops(i).dst_rtype, issue_unit.io.iss_uops(i).ldst,issue_unit.io.iss_uops(i).pdst,issue_unit.io.iss_uops(i).ldst_val.asUInt)
         printf("HfpPipeline--Issue Stage---------------------------------------------\n")
       }
   }

   // Wakeup
   for ((writeback, issue_wakeup) <- io.wakeups zip issue_unit.io.wakeup_pdsts)
   {
      issue_wakeup.valid := writeback.valid
      issue_wakeup.bits  := writeback.bits.uop.pdst
   }

   //-------------------------------------------------------------
   // **** Register Read Stage ****
   //-------------------------------------------------------------

   // Register Read <- Issue (rrd <- iss)
   fregister_read.io.rf_read_ports <> fregfile.io.read_ports

   fregister_read.io.iss_valids <> iss_valids
   fregister_read.io.iss_uops := iss_uops

   fregister_read.io.brinfo := io.brinfo
   fregister_read.io.kill := io.flush_pipeline

   if(DEBUG_PRINTF_HFPU){
      printf("HfpPipeline--Register Read---------------------------------------------\n")
      val rr_ports=exe_units.withFilter(_.uses_iss_unit).map(_.num_rf_read_ports).sum
      for(w <- 0 until rr_ports){
         printf("fregfile.io.read_ports.addr=[%d]    fregfile.io.read_ports.data=[%x]\n",
                 fregfile.io.read_ports(w).addr,fregfile.io.read_ports(w).data)
         printf("fregister_read.io.rf_read_ports.addr=[%d]    fregister_read.io.rf_read_ports.data=[%x]\n",
                 fregister_read.io.rf_read_ports(w).addr,fregister_read.io.rf_read_ports(w).data)
      }

      printf("    Iss_uops\n")
      for(w <- 0 until issue_unit.issue_width){
         printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    dis.valid=[%d]\n",
                 iss_uops(w).uopc,iss_uops(w).inst,
                 iss_uops(w).hfp_val.asUInt,iss_uops(w).fu_code,
                 iss_uops(w).iqtype,iss_uops(w).valid.asUInt,iss_valids(w).asUInt)
         printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]\n",
                 iss_uops(w).lrs1_rtype,iss_uops(w).lrs1,iss_uops(w).pop1, 
                 iss_uops(w).lrs2_rtype,iss_uops(w).lrs2,iss_uops(w).pop2,
                 iss_uops(w).lrs3_rtype,iss_uops(w).lrs3,iss_uops(w).pop3,
                 iss_uops(w).dst_rtype,iss_uops(w).ldst,iss_uops(w).pdst,iss_uops(w).ldst_val.asUInt)
      }
      printf("    fregister_read\n")
      for(w <- 0 until issue_unit.issue_width){
         printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    dis.valid=[%d]\n",
                 fregister_read.io.iss_uops(w).uopc,fregister_read.io.iss_uops(w).inst,
                 fregister_read.io.iss_uops(w).hfp_val.asUInt,fregister_read.io.iss_uops(w).fu_code,
                 fregister_read.io.iss_uops(w).iqtype,fregister_read.io.iss_uops(w).valid.asUInt,fregister_read.io.iss_valids.asUInt)
         printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]\n",
                 fregister_read.io.iss_uops(w).lrs1_rtype,fregister_read.io.iss_uops(w).lrs1,fregister_read.io.iss_uops(w).pop1, 
                 fregister_read.io.iss_uops(w).lrs2_rtype,fregister_read.io.iss_uops(w).lrs2,fregister_read.io.iss_uops(w).pop2,
                 fregister_read.io.iss_uops(w).lrs3_rtype,fregister_read.io.iss_uops(w).lrs3,fregister_read.io.iss_uops(w).pop3,
                 fregister_read.io.iss_uops(w).dst_rtype,fregister_read.io.iss_uops(w).ldst,fregister_read.io.iss_uops(w).pdst,fregister_read.io.iss_uops(w).ldst_val.asUInt)
      }

      printf("HfpPipeline--Register Read---------------------------------------------\n")
   }

   //-------------------------------------------------------------
   // **** Execute Stage ****
   //-------------------------------------------------------------

   exe_units.map(_.io.brinfo := io.brinfo)
   exe_units.map(_.io.com_exception := io.flush_pipeline)

   if(DEBUG_PRINTF_HFPU){
      printf("HfpPipeline--Execute Stage---------------------------------------------\n")
      printf("    fregister_read\n")
      for(w <- 0 until issue_unit.issue_width){
         printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]\n",
                 fregister_read.io.exe_reqs(w).bits.uop.uopc,fregister_read.io.exe_reqs(w).bits.uop.inst,
                 fregister_read.io.exe_reqs(w).bits.uop.hfp_val.asUInt,fregister_read.io.exe_reqs(w).bits.uop.fu_code,
                 fregister_read.io.exe_reqs(w).bits.uop.iqtype,fregister_read.io.exe_reqs(w).bits.uop.valid.asUInt)
         printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]\n",
                 fregister_read.io.exe_reqs(w).bits.uop.lrs1_rtype,fregister_read.io.exe_reqs(w).bits.uop.lrs1,fregister_read.io.exe_reqs(w).bits.uop.pop1, 
                 fregister_read.io.exe_reqs(w).bits.uop.lrs2_rtype,fregister_read.io.exe_reqs(w).bits.uop.lrs2,fregister_read.io.exe_reqs(w).bits.uop.pop2,
                 fregister_read.io.exe_reqs(w).bits.uop.lrs3_rtype,fregister_read.io.exe_reqs(w).bits.uop.lrs3,fregister_read.io.exe_reqs(w).bits.uop.pop3,
                 fregister_read.io.exe_reqs(w).bits.uop.dst_rtype, fregister_read.io.exe_reqs(w).bits.uop.ldst,fregister_read.io.exe_reqs(w).bits.uop.pdst,fregister_read.io.exe_reqs(w).bits.uop.ldst_val.asUInt)
      }
     printf("HfpPipeline--Execute Stage---------------------------------------------\n")
   }

   for ((ex,w) <- exe_units.withFilter(_.uses_iss_unit).map(x=>x).zipWithIndex)
   {
      ex.io.req <> fregister_read.io.exe_reqs(w)
      require (!ex.isBypassable)

      if(DEBUG_PRINTF_HFPU){
         printf("HfpPipeline--Execute Stage---------------------------------------------\n")
         printf("    Exe_units\n")
         printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]\n",
                 ex.io.req.bits.uop.uopc,ex.io.req.bits.uop.inst,
                 ex.io.req.bits.uop.hfp_val.asUInt,ex.io.req.bits.uop.fu_code,
                 ex.io.req.bits.uop.iqtype,ex.io.req.bits.uop.valid.asUInt)
         printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]\n",
                 ex.io.req.bits.uop.lrs1_rtype,ex.io.req.bits.uop.lrs1,ex.io.req.bits.uop.pop1, 
                 ex.io.req.bits.uop.lrs2_rtype,ex.io.req.bits.uop.lrs2,ex.io.req.bits.uop.pop2,
                 ex.io.req.bits.uop.lrs3_rtype,ex.io.req.bits.uop.lrs3,ex.io.req.bits.uop.pop3,
                 ex.io.req.bits.uop.dst_rtype,ex.io.req.bits.uop.ldst,ex.io.req.bits.uop.pdst,ex.io.req.bits.uop.ldst_val.asUInt)
          printf("HfpPipeline--Execute Stage---------------------------------------------\n")
       }


      if(DEBUG_PRINTF_HFPU){
         printf("HfpPipeline-Start--------------------------------------------------------------------------------------------\n")
         printf("fregister_read.io.exe_reqs.rs1=[%x]    fregister_read.io.exe_reqs.rs2=[%x]    fregister_read.io.exe_reqs.rs3=[%x]\n",
                 fregister_read.io.exe_reqs(w).bits.rs1_data,fregister_read.io.exe_reqs(w).bits.rs2_data,fregister_read.io.exe_reqs(w).bits.rs3_data);
         printf("ex.io.req.rs1=[%x]    ex.io.req.rs2=[%x]    ex.io.req.rs3=[%x]\n",ex.io.req.bits.rs1_data,ex.io.req.bits.rs2_data,ex.io.req.bits.rs3_data);
         printf("HfpPipeline-End--------------------------------------------------------------------------------------------\n")
      }


      // TODO HACK only let one FPU issue port issue these.
      require (w == 0)
      when (fregister_read.io.exe_reqs(w).bits.uop.uopc === uopSTD) {
         ex.io.req.valid :=  Bool(false)
      }

      io.tosdq.valid    := fregister_read.io.exe_reqs(w).bits.uop.uopc === uopSTD
      io.tosdq.bits.uop := fregister_read.io.exe_reqs(w).bits.uop
      val sdata = fregister_read.io.exe_reqs(w).bits.rs2_data

      //val unrec_s = hardfloat.fNFromRecFN(8, 24, sdata)
      //val unrec_d = hardfloat.fNFromRecFN(11, 53, sdata)
      val unrec_h = hardfloat.fNFromRecFN(5, 11, sdata)
      //val unrec_out = Mux(io.tosdq.bits.uop.fp_single, Cat(Fill(32, unrec_s(31)), unrec_s), unrec_d)
      val unrec_out = Cat(Fill(48, unrec_h(15)),unrec_h)

      io.tosdq.bits.data := unrec_out
   }
   require (exe_units.num_total_bypass_ports == 0)

   exe_units.ihfpu_unit.io.req <> io.fromint
   exe_units.fphfpu_unit.io.req <> io.fromfp

   //-------------------------------------------------------------
   // **** Writeback Stage ****
   //-------------------------------------------------------------

   val ll_wbarb = Module(new Arbiter(new ExeUnitResp(fLen+1), 3))
   val ihfpu_resp = exe_units.ihfpu_unit.io.resp(0)
   val fphfpu_resp = exe_units.fphfpu_unit.io.resp(0)

   // Hookup load writeback -- and recode FP values.
   ll_wbarb.io.in(0) <> io.ll_wport

   if(DEBUG_PRINTF_HFPU){
      printf("HfpPipeline--Writeback Stage--ll_wport-----------------------------------------\n")
      printf("    ll_wbarb.io.in(0)\n")
      printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    ll_wport.valid=[%d]\n",
              ll_wbarb.io.in(0).bits.uop.uopc,          ll_wbarb.io.in(0).bits.uop.inst,
              ll_wbarb.io.in(0).bits.uop.hfp_val.asUInt,ll_wbarb.io.in(0).bits.uop.fu_code,
              ll_wbarb.io.in(0).bits.uop.iqtype,        ll_wbarb.io.in(0).bits.uop.valid.asUInt,ll_wbarb.io.in(0).valid.asUInt)
      printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---ll_wbarb.io.in(0).%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]    wbdata=[%x]\n",
              ll_wbarb.io.in(0).bits.uop.lrs1_rtype, ll_wbarb.io.in(0).bits.uop.lrs1,ll_wbarb.io.in(0).bits.uop.pop1, 
              ll_wbarb.io.in(0).bits.uop.lrs2_rtype, ll_wbarb.io.in(0).bits.uop.lrs2,ll_wbarb.io.in(0).bits.uop.pop2,
              ll_wbarb.io.in(0).bits.uop.lrs3_rtype, ll_wbarb.io.in(0).bits.uop.lrs3,ll_wbarb.io.in(0).bits.uop.pop3,
              ll_wbarb.io.in(0).bits.uop.dst_rtype,  ll_wbarb.io.in(0).bits.uop.ldst,ll_wbarb.io.in(0).bits.uop.pdst,ll_wbarb.io.in(0).bits.uop.ldst_val.asUInt,ll_wbarb.io.in(0).bits.data)

      printf("    io.ll_wport\n")
      printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    ll_wport.valid=[%d]\n",
              io.ll_wport.bits.uop.uopc,io.ll_wport.bits.uop.inst,
              io.ll_wport.bits.uop.hfp_val.asUInt,io.ll_wport.bits.uop.fu_code,
              io.ll_wport.bits.uop.iqtype,io.ll_wport.bits.uop.valid.asUInt,io.ll_wport.valid.asUInt)
      printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]    wbdata=[%x]\n",
              io.ll_wport.bits.uop.lrs1_rtype,io.ll_wport.bits.uop.lrs1,io.ll_wport.bits.uop.pop1, 
              io.ll_wport.bits.uop.lrs2_rtype,io.ll_wport.bits.uop.lrs2,io.ll_wport.bits.uop.pop2,
              io.ll_wport.bits.uop.lrs3_rtype,io.ll_wport.bits.uop.lrs3,io.ll_wport.bits.uop.pop3,
              io.ll_wport.bits.uop.dst_rtype, io.ll_wport.bits.uop.ldst,io.ll_wport.bits.uop.pdst,io.ll_wport.bits.uop.ldst_val.asUInt,
              io.ll_wport.bits.data)
      printf("HfpPipeline--Writeback Stage--ll_wport-----------------------------------------\n")
   }


   val typ = io.ll_wport.bits.uop.mem_typ
   val load_single = typ === rocket.MT_W || typ === rocket.MT_WU
   //val rec_s = hardfloat.recFNFromFN( 8, 24, io.ll_wport.bits.data)
   //val rec_d = hardfloat.recFNFromFN(11, 53, io.ll_wport.bits.data)
   val rec_h = hardfloat.recFNFromFN(5, 11, io.ll_wport.bits.data)
   //val fp_load_data_recoded = Mux(load_single, Cat(SInt(-1, 32), rec_s), rec_d)
   val hfp_load_data_recoded = Cat(SInt(-1, 48), rec_h)
   ll_wbarb.io.in(0).bits.data := hfp_load_data_recoded
   if(DEBUG_PRINTF_HFPU){
      printf("HfpPipeline--Writeback Stage--ll_wbarb.io.in(0).bits.data\n")
      printf("ll_wbarb.io.in(0).bits.data=[%x]\n",ll_wbarb.io.in(0).bits.data)
      printf("HfpPipeline--Writeback Stage--ll_wbarb.io.in(0).bits.data\n")
   }
   ll_wbarb.io.in(1) <> ihfpu_resp
   ll_wbarb.io.in(2) <> fphfpu_resp
   ll_wbarb.io.in(1).valid := ihfpu_resp.valid && ihfpu_resp.bits.uop.dst_rtype === RT_FLT
   ll_wbarb.io.in(1).bits  := ihfpu_resp.bits
   ll_wbarb.io.in(2).valid := fphfpu_resp.valid && fphfpu_resp.bits.uop.dst_rtype === RT_FLT
   ll_wbarb.io.in(2).bits  := fphfpu_resp.bits
   if (regreadLatency > 0) {
      // Cut up critical path by delaying the write by a cycle.
      // Wakeup signal is sent on cycle S0, write is now delayed until end of S1,
      // but Issue happens on S1 and RegRead doesn't happen until S2 so we're safe.
      // (for regreadlatency >0).
      fregfile.io.write_ports(0) <> WritePort(RegNext(ll_wbarb.io.out), FPREG_SZ, fLen+1)
   } else {
      fregfile.io.write_ports(0) <> WritePort(ll_wbarb.io.out, FPREG_SZ, fLen+1)
   }

   assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.
   when (ihfpu_resp.valid) { assert (ihfpu_resp.bits.uop.ctrl.rf_wen && ihfpu_resp.bits.uop.dst_rtype === RT_FHT) }
   when (fphfpu_resp.valid) { assert (fphfpu_resp.bits.uop.ctrl.rf_wen && fphfpu_resp.bits.uop.dst_rtype === RT_FHT) }


   var w_cnt = 1
   var toint_found = false
   var tofp_found  = false
   for (eu <- exe_units)
   {
      for (wbresp <- eu.io.resp)
      {
         val toint = wbresp.bits.uop.dst_rtype === RT_FIX
         val tofp  = wbresp.bits.uop.dst_rtype === RT_FLT

         if (wbresp.bits.writesToIRF || wbresp.bits.writesToFRF) {
            if(toint == true){
               io.toint <> wbresp
               assert(!(wbresp.valid && !toint))
               assert(!toint_found) // only support one toint connector
               toint_found = true
            } else {
               io.tofp <> wbresp
               assert(!(wbresp.valid && !tofp))
               assert(!tofp_found)
               tofp_found=true
            }
            io.toint.valid := wbresp.valid && toint
            io.tofp.valid  := wbresp.valid && tofp
         } else if (eu.has_ihfpu || eu.has_fphfpu) {
            // share with ll unit
         } else {
            assert (!(wbresp.valid && (toint || tofp)))
            fregfile.io.write_ports(w_cnt).valid :=
               wbresp.valid &&
               wbresp.bits.uop.ctrl.rf_wen
            fregfile.io.write_ports(w_cnt).bits.addr := wbresp.bits.uop.pdst
            fregfile.io.write_ports(w_cnt).bits.data := wbresp.bits.data
            wbresp.ready := fregfile.io.write_ports(w_cnt).ready
            if(DEBUG_PRINTF_HFPU){
               printf("HfpPipeline--Writeback Stage--write to hfp reg------------------------------------------------------------------------------------------\n")
               printf("    wbresp\n")
               printf("    wbresp.valid=[%d]    wbresp.bits.uop.ctrl.rf_wen=[%d]    wbresp.bits.uop.pdst=[%x]    wbresp.bits.data=[%x]    wbresp.ready=[%d]\n",
                       wbresp.valid.asUInt, wbresp.bits.uop.ctrl.rf_wen,        wbresp.bits.uop.pdst,        wbresp.bits.data,        wbresp.ready.asUInt);
               printf("     wbresp.bits.uop.uopc=[%d]    wbresp.bits.uop.fu_code=[%d]\n",wbresp.bits.uop.uopc,wbresp.bits.uop.fu_code)
               printf("     fregfile.io.write_ports(%d)\n",UInt(w_cnt))
               printf("     fregfile.io.write_ports.valid=[%d]    fregfile.io.write_ports.bits.addr=[%x]    fregfile.io.write_ports.bits.data=[%x]    fregfile.io.write_ports.ready=[%d]\n",
                       fregfile.io.write_ports(w_cnt).valid.asUInt,fregfile.io.write_ports(w_cnt).bits.addr,fregfile.io.write_ports(w_cnt).bits.data,fregfile.io.write_ports(w_cnt).ready.asUInt);
               printf("HfpPipeline--Writeback stage--write to hfp reg--------------------------------------------------------------------------------------------\n")
            }
         }

         assert (!(wbresp.valid &&
            !wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype === RT_FHT),
            "[hfppipeline] An HFP writeback is being attempted with rf_wen disabled.")
         if(DEBUG_PRINTF_HFPU){
            printf("HfpPipeline--Writeback Stage--assert---\n")
            printf("wbresp.valid=[%d]    wbresp.bits.uop.ctrl.rf_wen=[%d]    wbresp.bits.uop.dst_rtype=[%d]",
                    wbresp.valid.asUInt, wbresp.bits.uop.ctrl.rf_wen.asUInt, wbresp.bits.uop.dst_rtype)
            printf("HfpPipeline--Writeback Stage--assert---\n")
         }

         assert (!(wbresp.valid &&
            wbresp.bits.uop.ctrl.rf_wen &&
            wbresp.bits.uop.dst_rtype =/= RT_FHT &&
            !toint && !tofp),
            "[hfppipeline] A writeback is being attempted to the HFP RF with dst != HFP type.")

         if (!wbresp.bits.writesToIRF && !(eu.has_ihfpu || eu.has_fphfpu) && !wbresp.bits.writesToFRF) w_cnt += 1
      }
   }
   require (w_cnt == fregfile.io.write_ports.length)


   //-------------------------------------------------------------
   //-------------------------------------------------------------
   // **** Commit Stage ****
   //-------------------------------------------------------------
   //-------------------------------------------------------------

   io.wakeups(0) <> ll_wbarb.io.out
   ll_wbarb.io.out.ready := Bool(true)
   assert( !(io.wakeups(0).valid) || (io.wakeups(0).valid && io.wakeups(0).bits.uop.dst_rtype === RT_FHT))
   if(DEBUG_PRINTF_HFPU){
      printf("HfpPipeline--ll_wport-----------------------------------------\n")
      printf("    ll_wbarb.io.out\n")
      printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    ll_wport.valid=[%d]\n",
              ll_wbarb.io.out.bits.uop.uopc,          ll_wbarb.io.out.bits.uop.inst,
              ll_wbarb.io.out.bits.uop.hfp_val.asUInt,ll_wbarb.io.out.bits.uop.fu_code,
              ll_wbarb.io.out.bits.uop.iqtype,        ll_wbarb.io.out.bits.uop.valid.asUInt,ll_wbarb.io.out.valid.asUInt)
      printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---ll_wbarb.io.in(0).%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]    wbdata=[%x]\n",
              ll_wbarb.io.out.bits.uop.lrs1_rtype, ll_wbarb.io.out.bits.uop.lrs1,ll_wbarb.io.out.bits.uop.pop1, 
              ll_wbarb.io.out.bits.uop.lrs2_rtype, ll_wbarb.io.out.bits.uop.lrs2,ll_wbarb.io.out.bits.uop.pop2,
              ll_wbarb.io.out.bits.uop.lrs3_rtype, ll_wbarb.io.out.bits.uop.lrs3,ll_wbarb.io.out.bits.uop.pop3,
              ll_wbarb.io.out.bits.uop.dst_rtype,  ll_wbarb.io.out.bits.uop.ldst,ll_wbarb.io.out.bits.uop.pdst,ll_wbarb.io.out.bits.uop.ldst_val.asUInt,ll_wbarb.io.out.bits.data)

      printf("    io.wakeups(0)\n")
      printf("uop.uopc=[%d]    uop.inst=[%x]    uop.hfp_val=[%d]   uop.fu_code=[%d]    uop.iqtype=[%d]    uop.valid=[%d]    ll_wport.valid=[%d]\n",
              io.wakeups(0).bits.uop.uopc,io.wakeups(0).bits.uop.inst,
              io.wakeups(0).bits.uop.hfp_val.asUInt,io.wakeups(0).bits.uop.fu_code,
              io.wakeups(0).bits.uop.iqtype,io.wakeups(0).bits.uop.valid.asUInt,io.wakeups(0).valid.asUInt)
      printf("uop.lrs1=[%d---%d---%d]    uop.lrs2=[%d---%d---%d]    uop.lrs3=[%d---%d---%d]    uop.dst=[%d---%d---%d---%d]    wbdata=[%x]\n",
              io.wakeups(0).bits.uop.lrs1_rtype,io.wakeups(0).bits.uop.lrs1,io.wakeups(0).bits.uop.pop1, 
              io.wakeups(0).bits.uop.lrs2_rtype,io.wakeups(0).bits.uop.lrs2,io.wakeups(0).bits.uop.pop2,
              io.wakeups(0).bits.uop.lrs3_rtype,io.wakeups(0).bits.uop.lrs3,io.wakeups(0).bits.uop.pop3,
              io.wakeups(0).bits.uop.dst_rtype, io.wakeups(0).bits.uop.ldst,io.wakeups(0).bits.uop.pdst,io.wakeups(0).bits.uop.ldst_val.asUInt,
              io.wakeups(0).bits.data)
      printf("HfpPipeline--ll_wport-----------------------------------------\n")
   }


   w_cnt = 1
   for (eu <- exe_units)
   {
      for (exe_resp <- eu.io.resp)
      {
         val wb_uop = exe_resp.bits.uop

         if (!exe_resp.bits.writesToIRF && !(eu.has_ihfpu || eu.has_fphfpu) && !exe_resp.bits.writesToFRF) {
            val wport = io.wakeups(w_cnt)
            wport <> exe_resp
            wport.valid := exe_resp.valid && wb_uop.dst_rtype === RT_FHT

            w_cnt += 1

            assert(!(exe_resp.valid && wb_uop.is_store))
            assert(!(exe_resp.valid && wb_uop.is_load))
            assert(!(exe_resp.valid && wb_uop.is_amo))
         }
      }
   }

   for ( i <- 0 until io.wakeups.length)
   {
      assert( !(io.wakeups(i).valid) || (io.wakeups(i).valid && io.wakeups(i).bits.uop.dst_rtype === RT_FHT))
   }



   exe_units.map(_.io.fcsr_rm := io.fcsr_rm)

   //-------------------------------------------------------------
   // **** Flush Pipeline ****
   //-------------------------------------------------------------
   // flush on exceptions, miniexeptions, and after some special instructions

   for (w <- 0 until exe_units.length)
   {
      exe_units(w).io.req.bits.kill := io.flush_pipeline
   }

   if(DEBUG_PRINTF_REGF)
   {
      printf("hfp-physical-registers--------------------------------------\n")
      for(i <- 0 until numFpPhysRegs)
      {
         printf("    hprs[%d]=[%x]    ",UInt(i,log2Up(numFpPhysRegs)),fregfile.io.debug(i).data)
         if((i+1)%4==0)printf("\n")
      }
      printf("\n")
   }
}
