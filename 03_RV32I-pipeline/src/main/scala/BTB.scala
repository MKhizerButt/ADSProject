package core_tile

import chisel3._
import chisel3.util._

// FSM States for 2-bit saturating counter
object BTBStates extends ChiselEnum {
    val strongNotTaken,  weakNotTaken,  weakTaken, strongTaken  = Value
}

class BTB extends Module {
    val io = IO(new Bundle {
        val PC = Input(UInt(32.W))
        val update = Input(Bool())
        val updatePC = Input(UInt(32.W))
        val updateTarget = Input(UInt(32.W))
        val mispredicted = Input(Bool())

        val valid = Output(Bool())
        val target = Output(UInt(32.W))
        val predictTaken = Output(Bool())
    })

  import BTBStates._
  
  // Parameters for BTB configuration
  val numSets = 8
  val numWays = 2
  val indexBits = log2Ceil(numSets)
  val tagBits = 32 - indexBits - 2 // Last 2 bits are for Byte offset as standard computer memory is byte-addressable, meaning every single byte has its own unique address.
  // Above the 2 ways are not subtracted as in Ser-Associative both ways will be compared in parallel
  
  // BTB Memory Arrays (8 sets, 2 ways)
  // validRegs(set)(way)
  val validRegs  = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(numWays)(false.B)))))
  val tagRegs    = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(numWays)(0.U(tagBits.W))))))
  val targetRegs = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(numWays)(0.U(32.W))))))
  
  // Initialize state to weakTaken 
  val predictState  = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(numWays)(BTBStates.weakTaken)))))

  // LRU Tracker: 1 bit per set. 0 means Way 0 is LRU (oldest). 1 means Way 1 is LRU.
  val lruRegs    = RegInit(VecInit(Seq.fill(numSets)(0.U(1.W))))

  // Extract index and tag from input PC
  val readIndex = io.PC(indexBits + 1, 2)   // Bits 4:2 - 3 bits for 8 sets, word-aligned so start from bit 2
  val readTag   = io.PC(31, indexBits + 2)  // Bits 31:5 - remaining 27 bits for tag

  val validWay0 = validRegs(readIndex)(0) && (tagRegs(readIndex)(0) === readTag)
  val validWay1 = validRegs(readIndex)(1) && (tagRegs(readIndex)(1) === readTag)

  // Output logic for prediction
  io.valid := validWay0 || validWay1
  io.target := Mux(validWay0, targetRegs(readIndex)(0), Mux(validWay1, targetRegs(readIndex)(1), 0.U(32.W)))
  io.predictTaken := Mux(validWay0, predictState(readIndex)(0) === strongTaken || predictState(readIndex)(0) === weakTaken, 
                    Mux(validWay1, predictState(readIndex)(1) === strongTaken || predictState(readIndex)(1) === weakTaken, false.B))

  // LRU update
  when(validWay0) {
    lruRegs(readIndex) := 1.U // Way 1 becomes LRU
  } .elsewhen(validWay1) {
    lruRegs(readIndex) := 0.U // Way 0 becomes LRU
  }

  def getNextState(currentState: BTBStates.Type, mispredict: Bool): BTBStates.Type = { // Input inside (), Output after :
    val predTaken = (currentState === strongTaken || currentState === weakTaken)

    val actuTaken = Mux(mispredict, !predTaken, predTaken) // Actual taken is opposite of predicted if mispredicted

    val nextState = WireDefault(weakTaken) // Default next state to weakTaken to avoid latches

    when(actuTaken) {
      switch(currentState) {
        is(strongTaken) { nextState := strongTaken }
        is(weakTaken) { nextState := strongTaken }
        is(weakNotTaken) { nextState := strongTaken }
        is(strongNotTaken) { nextState := weakNotTaken }
        }
    } .otherwise {
      switch(currentState) {
        is(strongTaken) { nextState := weakTaken }
        is(weakTaken) { nextState := strongNotTaken }
        is(weakNotTaken) { nextState := strongNotTaken }
        is(strongNotTaken) { nextState := strongNotTaken }
      }
    }

    nextState
  }
     
}