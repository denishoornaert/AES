package aes

import spinal.core._
import spinal.core.sim._

object AESSim extends App {
  Config.sim.compile{
      val dut = new AES(128)
      dut.steps(0).simPublic()
      dut
    }.doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)
    dut.io.source.valid #= true
    dut.io.destination.ready #= true
    dut.io.source.payload #= BigInt("101112131415161718191A1B1C1D1E1F", 16)
    for(i <- 0 to 3) {
      dut.clockDomain.waitSampling()
    }
  }
}
