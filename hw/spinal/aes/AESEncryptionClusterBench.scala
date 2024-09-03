package aes

import java.io._

import spinal.core._
import spinal.core.sim._

object AESEncryptionClusterBench extends App {

  case class BenchEntry(cores: Int, blocks: Int, clocks: Int, rate: Double) {

    def header(): String = {
      return "cores, blocks, clocks, rate\n"
    }

    def asString(): String = {
      return s"$cores, $blocks, $clocks, $rate\n"
    }

  }

  var frame = Seq[BenchEntry]()

  for (c <- 1 to 8) {
    val compiled = Config.sim.compile(new AESCluster(message_width=128, key_width=128, metadata_width=6, encrypts=true, cores=c))

    compiled.doSim(s"cores_${c}") { dut =>
      dut.clockDomain.forkStimulus(period = 10)

      val key = BigInt("5468617473206D79204B756E67204675", 16)
      val msg = BigInt("00000000000000000000000000000000", 16)

      val blocks_to_encrypt = 256
      var clock_count = 0
    
      dut.io.destination.ready #= true

      dut.clockDomain.waitRisingEdge(8)

      dut.clockDomain.onRisingEdges{ clock_count = clock_count+1 }

      val produce = fork {
        for (i <- 0 until blocks_to_encrypt) {
          dut.io.source.valid              #= true
          dut.io.source.payload.message    #= msg+i
          dut.io.source.payload.key        #= key
          dut.io.source.payload.metadata   #= i%(64)

          dut.clockDomain.waitRisingEdgeWhere(dut.io.source.ready.toBoolean)
          dut.io.source.valid #= false
        }
      }

      val consume = fork {
        for (i <- 0 until blocks_to_encrypt) {
          dut.clockDomain.waitRisingEdgeWhere(dut.io.destination.valid.toBoolean)
        }
      }

      produce.join()
      consume.join()

      frame = frame :+ BenchEntry(cores=c, blocks=blocks_to_encrypt, clocks=clock_count, rate=blocks_to_encrypt.toDouble/clock_count.toDouble)
    }
  }

  val fileWriter = new FileWriter(new File("hw/gen/cluster_bandwidth_benchmark.csv"))
  fileWriter.write(frame(0).header())
  for (entry <- frame) {
    println(s"For ${entry.cores} cores: ${entry.blocks} blocks encrypted in ${entry.clocks} clock cycles. ${entry.rate} bpc")
    fileWriter.write(entry.asString())
  }
  fileWriter.close()

}
