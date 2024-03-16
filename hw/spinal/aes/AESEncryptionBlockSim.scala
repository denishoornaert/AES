package aes

import spinal.core._
import spinal.core.sim._

object AESEncryptionBlockSim extends App {
  Config.sim.compile{
      val dut = new AESEncryptionBlock(128)
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

    val encrypteds = Array(
      //     "T w o   O n e   N i n e   T w o "
      BigInt("54776F204F6E65204E696E652054776F", 16),
      BigInt("5847088B15B61CBA59D4E2E8CD39DFCE", 16),
      BigInt("43C6A9620E57C0C80908EBFE3DF87F37", 16),
      BigInt("7876305470767D23993C375B4B3934F1", 16),
      BigInt("B1CA51ED08FC54E104B1C9D3E7B26C20", 16),
      BigInt("9B512068235F22F05D1CBD322F389156", 16),
      BigInt("149325778FA42BE8C06024405E0F9275", 16),
      BigInt("53398E5D430693F84F0A3B95855257BD", 16),
      BigInt("66253C7470CE5AA8AFD30F0AA3731354", 16),
      BigInt("09668B78A2D19A65F0FCE6C47B3B3089", 16),
      BigInt("29C3505F571420F6402299B31A02D73A", 16)
    )

    var key = BigInt("00000000000000000000000000000000", 16)
    var encrypted = BigInt("00000000000000000000000000000000", 16)
    var round = 0
    var constant = 0

    dut.io.source.valid #= true
    dut.io.source.payload.key #= keys(0)
    dut.io.source.payload.round #= round
    dut.io.source.payload.constant #= constant
    dut.io.source.payload.message #= encrypteds(0)
    
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
      encrypted = dut.io.destination.payload.message.toBigInt
      dut.io.source.payload.message #= encrypted
      println("ENC: "+dut.io.destination.payload.message.toBigInt.toString(16)+" = "+encrypteds(i).toString(16))
      assert(dut.io.destination.payload.message.toBigInt == encrypteds(i))
      // constant
      round = round+1
      dut.io.source.payload.round #= round
      // constant
      constant = dut.io.destination.payload.constant.toInt
      dut.io.source.payload.constant #= constant
      // next cc
      dut.clockDomain.waitSampling()
      println("OK")
    }
    println("--------------------------------------------------------------------------------")
  }
}
