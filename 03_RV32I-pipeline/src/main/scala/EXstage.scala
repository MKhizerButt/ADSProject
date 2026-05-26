// ADS I Class Project
// Pipelined RISC-V Core - EX Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
Instruction Execute (EX) Stage: ALU operations and exception detection

Instantiated Modules:
    ALU: Integrate your module from Assignment02 for arithmetic/logical operations

ALU Interface:
    alu.io.operandA: first operand input
    alu.io.operandB: second operand input
    alu.io.operation: operation code controlling ALU function
    alu.io.aluResult: computation result output

Internal Signals:
    Map uopc codes to ALUOp values

Functionality:
    Map instruction uop to ALU operation code
    Pass operands to ALU
    Output results to pipeline

Outputs:
    aluResult: computation result from ALU
    exception: pass exception flag
*/

package core_tile

import chisel3._
import chisel3.util._
import Assignment02.{ALU, ALUOp}
import uopc._

// -----------------------------------------
// Execute Stage
// -----------------------------------------
class EX extends Module {
  val io = IO(new Bundle {
    val uop = Input(UInt(7.W))
    val rd = Input(UInt(5.W))
    val operandA = Input(UInt(32.W))
    val operandB = Input(UInt(32.W))
    val xcptInvalid = Input(Bool())

    val inPC = Input(UInt(32.W)) // Input for current PC from ID stage for observation
    val inTargetPC = Input(UInt(32.W)) // Input for target PC from ID stage for branch/jump instructions

    val inBTBPredictTaken = Input(Bool()) // Input for predicted taken signal from ID stage for observation
    val inBTBPredictTarget = Input(UInt(32.W)) // Input for predicted target address from ID stage for observation
  
    val outRD = Output(UInt(5.W))
    val aluResult = Output(UInt(32.W))
    val exception = Output(Bool())
    val isBranch = Output(Bool()) // Output signal to indicate if the instruction is a branch/jump, will be used in EX stage for branch decision and in IF stage for PC update

    val btbUpdate = Output(Bool()) // Output signal to indicate if the BTB should be updated (for branch instructions)
    val btbUpdatePC = Output(UInt(32.W)) // Output for the PC of the branch instruction to update the BTB
    val btbTarget = Output(UInt(32.W)) // Output for the target address to update the BTB
    val btbMispredicted = Output(Bool()) // Output signal to indicate if there was a branch misprediction (for branch instructions)
  })

val alu = Module(new ALU())

val isCondBranch = WireDefault(false.B) // Set to true for conditional branches (BEQ, BNE, BLT, BGE, BLTU, BGEU)
val isUncondJump = WireDefault(false.B) // Set to true for unconditional jumps (JAL, JALR)
val actualTaken = WireDefault(false.B) // Default to not taken, will be set for branch instructions if condition is met

alu.io.operandA := io.operandA
alu.io.operandB := io.operandB

io.outRD := io.rd 
io.aluResult := alu.io.aluResult
io.exception := io.xcptInvalid // Pass exception flag from ID stage to output

alu.io.operation := ALUOp.ADD // Default operation to avoid latches

switch(uopc(io.uop(4, 0))) { // Use lower 5 bits of uop for instruction decoding as per current uopc encoding 
  is(uopc.ADD) { alu.io.operation := ALUOp.ADD }
  is(uopc.SUB) { alu.io.operation := ALUOp.SUB }
  is(uopc.XOR) { alu.io.operation := ALUOp.XOR }
  is(uopc.OR)  { alu.io.operation := ALUOp.OR  }
  is(uopc.AND) { alu.io.operation := ALUOp.AND }
  is(uopc.SLL) { alu.io.operation := ALUOp.SLL }
  is(uopc.SRL) { alu.io.operation := ALUOp.SRL }
  is(uopc.SRA) { alu.io.operation := ALUOp.SRA }
  is(uopc.SLT) { alu.io.operation := ALUOp.SLT }
  is(uopc.SLTU) { alu.io.operation := ALUOp.SLTU }

  is(uopc.ADDI) { alu.io.operation := ALUOp.ADD }
  is(uopc.XORI) { alu.io.operation := ALUOp.XOR }
  is(uopc.ORI)  { alu.io.operation := ALUOp.OR  }
  is(uopc.ANDI) { alu.io.operation := ALUOp.AND }
  is(uopc.SLLI) { alu.io.operation := ALUOp.SLL }
  is(uopc.SRLI) { alu.io.operation := ALUOp.SRL }
  is(uopc.SRAI) { alu.io.operation := ALUOp.SRA }
  is(uopc.SLTI) { alu.io.operation := ALUOp.SLT }
  is(uopc.SLTIU) { alu.io.operation := ALUOp.SLTU }

  is(uopc.JAL) { 
    alu.io.operation := ALUOp.ADD // For JAL, ALU will be used to calculate return address (PC + 4), so we can use ADD operation
    actualTaken := true.B // JAL is an unconditional jump, always taken
    isUncondJump := true.B
  }
  is(uopc.JALR) { 
    alu.io.operation := ALUOp.ADD // For JALR, ALU will be used to calculate target address (rs1 + imm), so we can use ADD operation
    actualTaken := true.B // JALR is an unconditional jump, always taken
    isUncondJump := true.B
  }

  is(uopc.BEQ) { 
      isCondBranch := true.B
      actualTaken   := (io.operandA === io.operandB) 
    }
    is(uopc.BNE) { 
      isCondBranch := true.B
      actualTaken   := (io.operandA =/= io.operandB) 
    }
    is(uopc.BLT) { 
      isCondBranch := true.B
      actualTaken   := (io.operandA.asSInt < io.operandB.asSInt) 
    }
    is(uopc.BGE) { 
      isCondBranch := true.B
      actualTaken   := (io.operandA.asSInt >= io.operandB.asSInt) 
    }
    is(uopc.BLTU) { 
      isCondBranch := true.B
      actualTaken   := (io.operandA < io.operandB) 
    }
    is(uopc.BGEU) { 
      isCondBranch := true.B
      actualTaken   := (io.operandA >= io.operandB) 
    }
}  
  
  // Detect branch misprediction:
  val predictedTaken = io.inBTBPredictTaken
  val targetMismatch = (io.inTargetPC =/= io.inBTBPredictTarget) && actualTaken // Target from ID stage doesn't match target from BTB when the branch is actually taken
  val mispredicted   = isCondBranch && ((actualTaken =/= predictedTaken) || targetMismatch) // Initially predictedTaken is false in the BTB, so first time it will be a misprediction

  // CRITICAL FLUSH CONTROL SIGNAL:
  // Redirect pipeline if it's an unconditional jump OR a mispredicted conditional branch!
  io.isBranch := mispredicted

  // BTB Update Logic
  io.btbUpdate := isCondBranch // Only update BTB for conditional branches
  io.btbUpdatePC := io.inPC
  io.btbTarget := io.inTargetPC
  io.btbMispredicted := mispredicted
}
//ToDo: Add your implementation according to the specification above here 