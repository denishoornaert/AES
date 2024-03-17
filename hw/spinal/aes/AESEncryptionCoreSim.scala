package aes

import spinal.core._
import spinal.core.sim._

object AESEncryptionCoreSim extends App {
  Config.sim.compile {
    val dut = AESEncryptionCore(128, 128)
    dut
  }.doSim { dut =>
    dut.clockDomain.forkStimulus(period = 10)

    val key = BigInt("5468617473206D79204B756E67204675", 16)

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
      BigInt("c5db3936c7ec4e5d97610607336822b1", 16),
    )

    for (i <- 0 until 8) {
      println("--------------------------------------------------------------------------------")
      println("Block "+i+" encryption:")
      dut.io.source.valid      #= true
      dut.io.source.message    #= msgs(i)
      dut.io.source.key        #= key
      dut.io.destination.ready #= true

      dut.clockDomain.waitSamplingWhere(dut.io.source.ready.toBoolean)
      dut.io.source.valid #= false

      dut.clockDomain.waitSamplingWhere(dut.io.destination.valid.toBoolean)
      assert(dut.io.destination.payload.message.toBigInt == encs(i))
      println("\t-> PASSES") 
    }
    println("--------------------------------------------------------------------------------")
    
  }
}
