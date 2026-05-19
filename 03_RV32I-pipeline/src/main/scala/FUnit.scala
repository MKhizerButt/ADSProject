package core_tile

import chisel3._
import chisel3.util._

// -----------------------------------------
// Forwarding Unit
// -----------------------------------------
class ForwardingUnit extends Module{
  val io = IO(new Bundle {
    val exBarRd = Input(UInt(5.W))
    val memBarRd = Input(UInt(5.W))

    val idBarRegFileReq_A = Input(UInt(5.W)) // RegisterRs
    val idBarRegFileReq_B = Input(UInt(5.W)) // RegisterRt
    
    val exBarWrEn = Input(Bool())   // NEW: Is the instruction in MEM writing?
    val wbStageWrEn = Input(Bool()) // Is the instruction in WB writing?    //     val memBarWrEn  = Input(Bool()) // Does not exist currently, write enable is only sent via wb stage to the regFile
    // no mem module in task 3, hence there were no load instr (or store), write enable was always "set"
    // Also no branche or jump as of now

    val regSelect_Rs = Output(UInt(2.W))
    val regSelect_Rt = Output(UInt(2.W))
    })

  io.regSelect_Rs := 0.U // Default from regFile
  io.regSelect_Rt := 0.U
  
  // EX Hazard
  when(io.idBarRegFileReq_A === io.exBarRd && io.idBarRegFileReq_A =/= 0.U && io.exBarWrEn) {
    io.regSelect_Rs := 1.U
  }
  // MEM Hazard 
  .elsewhen(io.idBarRegFileReq_A === io.memBarRd && io.idBarRegFileReq_A =/= 0.U && io.wbStageWrEn) {
    when(io.exBarRd =/= io.idBarRegFileReq_A) {
      io.regSelect_Rs := 2.U
    }
  }

  when(io.idBarRegFileReq_B === io.exBarRd && io.idBarRegFileReq_B =/= 0.U && io.exBarWrEn) {
    io.regSelect_Rt := 1.U
  }
  .elsewhen(io.idBarRegFileReq_B === io.memBarRd && io.idBarRegFileReq_B =/= 0.U && io.wbStageWrEn) {
    when(io.exBarRd =/= io.idBarRegFileReq_B) {
      io.regSelect_Rt := 2.U
    }
  }
}