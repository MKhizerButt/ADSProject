// ADS I Class Project
// Pipelined RISC-V Core - IF Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
The Instruction Fetch (IF) stage is the first stage of the pipeline and handles instruction retrieval from memory.

Memory:
    IMem: instruction memory with 4096 32-bit unsigned integer entires, loaded from a binary file at compile time

Internal Registers:
    PC: 32-bit unsigned integer register, initialized to 0 holding the current program counter address

Internal Signals:
    none

Functionality:
    Fetch the instruction at the current PC (word-aligned addressing)
    Increment the PC (word-aligned) each clock cycle to fetch the next sequential instruction

Parameters:
    BinaryFile: String - path to the binary file to load into instruction memory

Inputs:
    none

Outputs:
    instr: send the fetched instruction to IF Barrier
*/

package core_tile

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile

// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IF (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    // ToDo: Add I/O ports
    val instr = Output(UInt(32.W)) // Output for the fetched instruction to the IF Barrier
    val outPC = Output(UInt(32.W)) // Output for the current program counter 

    val target_pc = Input(UInt(32.W)) // Input for target PC from EX stage
    val pcSel = Input(Bool()) // Input for PC selection signal for branch from EX Stage /for Jump from ID Stage

    val btbValid = Input(Bool()) // Input for BTB valid signal from EX stage
    val btbTarget = Input(UInt(32.W)) // Input for BTB predicted
    val btbPredictTaken = Input(Bool()) // Input for BTB prediction signal from EX stage

    val outBTBPredictTaken = Output(Bool()) // Output for the predicted taken signal to the IF Barrier (for observation)
    val outBTBPredictTarget = Output(UInt(32.W)) // Output for the predicted target address to the IF Barrier (for observation)
  })

//ToDo: Add your implementation according to the specification above here 
  val pc_reg = RegInit(0.U(32.W)) // Program Counter register initialized to 0

  val iMem = Mem(4096, UInt(32.W)) // Instruction memory with 4096 entries initialized to 0

  val takePrediction = io.btbPredictTaken && io.btbValid // Determine if we should take the BTB prediction

  loadMemoryFromFile(iMem, BinaryFile) // Load instruction memory from binary file at compile time
  
  io.instr := iMem(pc_reg >> 2) // Fetch instruction at current PC (word-aligned, so shift right by 2)
  io.outPC := pc_reg

  io.outBTBPredictTaken := takePrediction // Output the predicted taken signal to the IF Barrier for observation
  io.outBTBPredictTarget := io.btbTarget // Output the predicted target address to the IF Barrier for observation

  // Fetch instruction: extract bits 13 down to 2 to get a 12-bit word-aligned index
  //io.instr := iMem(pc_reg(13, 2))

  when(io.pcSel) { // If PC selection signal is set (for branch/jump), update PC with offset
    pc_reg := io.target_pc
  } .elsewhen(takePrediction) { // If BTB predicts taken, update PC with predicted target
    pc_reg := io.btbTarget
  } .otherwise { // Otherwise, increment PC to fetch next sequential instruction
    pc_reg := pc_reg + 4.U
  }

}
