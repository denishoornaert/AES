package aes

import spinal.core._
import spinal.core.sim._

object AESKeyExpansionSim extends App {
  Config.sim.compile{
      val dut = new KeyExpansion(128, true)
      dut
    }.doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)

    val keys = Array(
      //     "T h a t s   m y   K u n g   F u "
      BigInt("5468617473206D79204B756E67204675", 16),
      BigInt("E232FCF191129188B159E4E6D679A293", 16),
      BigInt("56082007C71AB18F76435569A03AF7FA", 16),
      BigInt("D2600DE7157ABC686339E901C3031EFB", 16),
      BigInt("A11202C9B468BEA1D75157A01452495B", 16),
      BigInt("B1293B3305418592D210D232C6429B69", 16),
      BigInt("BD3DC287B87C47156A6C9527AC2E0E4E", 16),
      BigInt("CC96ED1674EAAA031E863F24B2A8316A", 16),
      BigInt("8E51EF21FABB4522E43D7A0656954B6C", 16),
      BigInt("BFE2BF904559FAB2A16480B4F7F1CBD8", 16),
      BigInt("28FDDEF86DA4244ACCC0A4FE3B316F26", 16)
    )

    var key = BigInt("00000000000000000000000000000000", 16)
    var round = 0

    dut.io.source.valid #= true
    dut.io.source.payload.key #= keys(0)
    dut.io.source.payload.round #= round
    
    dut.io.destination.ready #= true
    dut.clockDomain.waitSampling()
    
    for (i <- 1 until 10) {
      println("--------------------------------------------------------------------------------")
      println("ROUND "+i.toString)
      // key
      key = dut.io.destination.payload.key.toBigInt
      dut.io.source.payload.key #= key
      println("KEY: "+dut.io.destination.payload.key.toBigInt.toString(16)+" = "+keys(i).toString(16))
      assert(dut.io.destination.payload.key.toBigInt == keys(i))
      // constant
      round = round+1
      dut.io.source.payload.round #= round
      // next cc
      dut.clockDomain.waitSampling()
      println("OK")
    }
    println("--------------------------------------------------------------------------------")
  }
}
