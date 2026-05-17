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
  
    val outRD = Output(UInt(5.W))
    val aluResult = Output(UInt(32.W))
    val exception = Output(Bool())

    val isBranch = Output(Bool()) // Output signal to indicate if the instruction is a branch/jump, will be used in EX stage for branch decision and in IF stage for PC update
  })

val alu = Module(new ALU())

alu.io.operandA := io.operandA
alu.io.operandB := io.operandB

io.outRD := io.rd 
io.aluResult := alu.io.aluResult
io.exception := io.xcptInvalid // Pass exception flag from ID stage to output
io.isBranch := false.B // Default to not a branch/jump, will be set for branch instructions

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

  is(uopc.JAL) { alu.io.operation := ALUOp.ADD } // For JAL, ALU will be used to calculate return address (PC + 4), so we can use ADD operation
  is(uopc.JALR) { alu.io.operation := ALUOp.ADD } // For JALR, ALU will be used to calculate target address (rs1 + imm), so we can use ADD operation

  is{uopc.BEQ} { when (io.operandA === io.operandB) { io.isBranch := true.B } }
  is{uopc.BNE} { when (io.operandA =/= io.operandB) { io.isBranch := true.B } }
  is{uopc.BLT} { 
    when (io.operandA.asSInt < io.operandB.asSInt) { io.isBranch := true.B } 
  }
  is{uopc.BGE} { 
    when (io.operandA.asSInt >= io.operandB.asSInt) { io.isBranch := true.B } 
  }
  is{uopc.BLTU} { 
    when (io.operandA < io.operandB) { io.isBranch := true.B } 
  }
  is{uopc.BGEU} { 
    when (io.operandA >= io.operandB) { io.isBranch := true.B } 
  }
}
}
//ToDo: Add your implementation according to the specification above here 