package aes

import spinal.core._
import spinal.core.sim._

object AESEncryptionClusterSim extends App {
  Config.sim.compile {
    val dut = AESCluster(128, 128, true, 2)
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

    // These are teh 128 first character of 'lorem ipsum'
    val msgs = Array(
      BigInt("4c6f72656d20697073756d20646f6c6f", 16),
      BigInt("722073697420616d65742c20636f6e73", 16),
      BigInt("65637465747565722061646970697363", 16),
      BigInt("696e6720656c69742e2041656e65616e", 16),
      BigInt("20636f6d6d6f646f206c6967756c6120", 16),
      BigInt("6567657420646f6c6f722e2041656e65", 16),
      BigInt("616e206d617373612e2043756d20736f", 16),
      BigInt("63696973206e61746f7175652070656e", 16)
    )

    val encs = Array(
      BigInt("5a9e72a0123dbd7c36d2459fb5190770", 16),
      BigInt("b3ade675a0dad92caecdd3434c5a0176", 16),
      BigInt("0d14da8dd8b909e75b13b11061783b74", 16),
      BigInt("66ad8ac883b1c05de3247deebe17bd3f", 16),
      BigInt("161bf02326ebfffad7b54b5d556cdbc9", 16),
      BigInt("135daa3fd7cc54aabd84c9824d5e34a8", 16),
      BigInt("b7dc195739122523b108f5b00f3585df", 16),
      BigInt("c5db3936c7ec4e5d97610607336822b1", 16)
    )
    
    dut.io.destination.ready #= true

    dut.clockDomain.waitRisingEdge(8)

    val produce = fork {
      for (i <- 0 until 8) {
        println("--------------------------------------------------------------------------------")
        println("Block "+i+" encryption:")
        dut.io.source.valid             #= true
        dut.io.source.payload.message   #= msgs(i)
        dut.io.source.payload.key       #= keys(0)
        dut.io.destination.ready        #= true

        dut.clockDomain.waitRisingEdgeWhere(dut.io.source.ready.toBoolean)
        dut.io.source.valid #= false
      }
    }

    val consume = fork {
      for (i <- 0 until 8) {
        dut.clockDomain.waitRisingEdgeWhere(dut.io.destination.valid.toBoolean)
        println(dut.io.destination.payload.message.toBigInt.toString(16)+" == "+encs(i).toString(16))
        assert(dut.io.destination.payload.message.toBigInt == encs(i))
        println("\t-> PASSES") 
      }
    }

    produce.join()
    consume.join()

    println("--------------------------------------------------------------------------------")
    
  }
}
