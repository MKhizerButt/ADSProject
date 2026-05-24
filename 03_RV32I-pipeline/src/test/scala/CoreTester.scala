package core_tile

import chisel3._
import chiseltest._
import PipelinedRV32I._
import org.scalatest.flatspec.AnyFlatSpec

class CoreTester extends AnyFlatSpec with ChiselScalatestTester {
  "PipelinedRV32Icore" should "execute a loop and demonstrate branch prediction" in {
    
    // We pass the path of our new hex file to the core, and ask for a VCD waveform!
    test(new PipelinedRV32I("src/test/programs/btb_BinaryFile_pipelined")).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      
      // Step the clock 25 times to let the pipeline fetch, decode, and execute the entire loop
      dut.clock.setTimeout(0) // Disable timeout for this test
      dut.clock.step(40) // Step enough cycles to execute the loop and observe branch prediction behavior   
    }
  }
}