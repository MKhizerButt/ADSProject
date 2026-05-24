package core_tile

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BTBTester extends AnyFlatSpec with ChiselScalatestTester {
  "Branch Target Buffer" should "allocate, predict, update FSM, and evict LRU" in {
    test(new BTB()) { dut =>
      
      // Step 1: Check Empty State (Miss)
      dut.io.PC.poke(32.U) // Let's test PC 32
      dut.io.valid.expect(false.B) // Should be empty

      // Step 2: EX Stage Updates the BTB (First Time Taken)
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(32.U)
      dut.io.updateTarget.poke(100.U)
      dut.io.mispredicted.poke(true.B) // We missed it!
      dut.clock.step(1)
      dut.io.update.poke(false.B)

      // Step 3: IF Stage Reads it again (Hit!)
      dut.io.PC.poke(32.U)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(100.U)
      dut.io.predictTaken.expect(true.B) // FSM should initialize to weakTaken (predicts True)

      // Step 4: Prove the Hysteresis FSM (Two misses to flip)
      // EX Stage says: Actually, it was NOT TAKEN this time!
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(32.U)
      dut.io.mispredicted.poke(true.B) // Mispredicted (Guessed Taken, Reality Not Taken)
      dut.clock.step(1)
      
      // Look again. State should have dropped to Strong Not Taken (Hysteresis jump)
      dut.io.PC.poke(32.U)
      dut.io.valid.expect(true.B)
      dut.io.predictTaken.expect(false.B) // Prediction flips!
      dut.io.update.poke(false.B)

      // Step 5: Test LRU Eviction
      // Set 1 has PCs mapping to index 1 (Bits 4:2) -> PCs 4, 36, 68 all map to Index 1
      
      // Write PC 4 (Goes to Way 0)
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(4.U)
      dut.io.updateTarget.poke(200.U)
      dut.clock.step(1)
      dut.io.update.poke(false.B)
      
      // 2. SIMULATE PIPELINE FLUSH: Read PC 4!
      // This hit is required to flip the LRU pointer to Way 1
      dut.io.PC.poke(4.U)
      dut.io.valid.expect(true.B) 
      dut.clock.step(1)
      
      // Write PC 36 (Goes to Way 1)
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(36.U)
      dut.io.updateTarget.poke(300.U)
      dut.clock.step(1)
      dut.io.update.poke(false.B)

      // Read PC 4 to make Way 0 the Most Recently Used
      dut.io.PC.poke(4.U)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(200.U)
      dut.io.predictTaken.expect(true.B)
      dut.clock.step(1)

      // Write PC 68 (Must evict Way 1 because Way 0 was just used!)
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(68.U)
      dut.io.updateTarget.poke(400.U)
      dut.clock.step(1)
      dut.io.update.poke(false.B)

      // Verify PC 36 was evicted (Way 1 overwritten)
      dut.io.PC.poke(36.U)
      dut.io.valid.expect(false.B) 
      
      // Verify PC 68 exists
      dut.io.PC.poke(68.U)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(400.U)
      dut.io.predictTaken.expect(true.B)
    }
  }
}