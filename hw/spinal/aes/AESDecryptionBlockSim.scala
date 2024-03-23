package aes

import spinal.core._
import spinal.core.sim._

// A good source of comparison:
//    https://www.cryptool.org/en/cto/aes-step-by-step

object AESDecryptionBlockSim extends App {
  Config.sim.compile{
      val dut = new AESEncryptionBlock(128, 128, false)
      dut
    }.doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)

    val keys = Array(
      //     "T h a t s   m y   K u n g   F u "
      BigInt("28FDDEF86DA4244ACCC0A4FE3B316F26", 16),
      BigInt("BFE2BF904559FAB2A16480B4F7F1CBD8", 16),
      BigInt("8E51EF21FABB4522E43D7A0656954B6C", 16),
      BigInt("CC96ED1674EAAA031E863F24B2A8316A", 16),
      BigInt("BD3DC287B87C47156A6C9527AC2E0E4E", 16),
      BigInt("B1293B3305418592D210D232C6429B69", 16),
      BigInt("A11202C9B468BEA1D75157A01452495B", 16),
      BigInt("D2600DE7157ABC686339E901C3031EFB", 16),
      BigInt("56082007C71AB18F76435569A03AF7FA", 16),
      BigInt("E232FCF191129188B159E4E6D679A293", 16),
      BigInt("5468617473206D79204B756E67204675", 16)
    )

    val decrypteds = Array(
      BigInt("29c3505f571420f6402299b31a02d73a", 16),
      BigInt("338b762051667d92798febc20a3fbe67", 16),
      BigInt("ed6fe27a1a675b4c840019419712dc2a", 16),
      BigInt("fa49369d73d04ff5ba763f9b58dcf109", 16),
      BigInt("14cf7ab1269c81454c07b78c15d19323", 16),
      BigInt("c8b0ddb730c85055f237d1f894742066", 16),
      BigInt("bc389aa151eb1820ee120426b338ff39", 16),
      BigInt("1a5be99aab30d2aa0141d3e827b4babb", 16),
      BigInt("6a4e988b59489e3dcb1230f4bda09c9b", 16),
      BigInt("632fafa2eb93c7209f92abcba0c0302b", 16),
      BigInt("54776f204f6e65204e696e652054776f", 16)
      //     "T w o   O n e   N i n e   T w o "
    )

    var key = BigInt("00000000000000000000000000000000", 16)
    var decrypted = BigInt("00000000000000000000000000000000", 16)
    var round = 9

    dut.io.source.valid #= true
    dut.io.source.payload.key #= keys(1)
    dut.io.source.payload.round #= round
    dut.io.source.payload.message #= decrypteds(0)^keys(0) // Simulate initial AddRoundKey
    
    dut.io.destination.ready #= true
    dut.clockDomain.waitSampling()
    
    for (i <- 1 until 10) {
      println("--------------------------------------------------------------------------------")
      println("ROUND "+i.toString)
      // key
      dut.io.source.payload.key #= keys(i+1)
      // message
      decrypted = dut.io.destination.payload.message.toBigInt
      dut.io.source.payload.message #= decrypted
      println("DEC: "+dut.io.destination.payload.message.toBigInt.toString(16)+" = "+decrypteds(i).toString(16))
      assert(dut.io.destination.payload.message.toBigInt == decrypteds(i))
      // round number
      round = round-1
      dut.io.source.payload.round #= round
      // next cc
      dut.clockDomain.waitSampling()
      println("OK")
    }
    println("--------------------------------------------------------------------------------")
  }
}
