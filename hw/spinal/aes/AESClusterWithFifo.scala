package aes

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class AESClusterWithFifo(message_width: Int, key_width: Int, metadata_width: Int, encrypts: Boolean = true, cores: Int = 2) extends Component {
  this.setName(if (encrypts) "AESEncryptionCluster" else "AESDecryptionCluster")

  val threads = 4

  val io = new Bundle {
    val source      =  slave(Stream(CoreInterfaceIn(message_width, key_width, metadata_width)))
    val destination = master(Stream(CoreInterfaceOut(message_width, metadata_width)))
  }

  // Creates the aggregation of cores
  val cluster = Seq.fill(cores)(AESCoreWithFifo(message_width, key_width, metadata_width, encrypts, threads))

  // Checks for first available core to receive the cypherblock
  val (dummy0, toCoresSelection): (Bool, UInt) = Vec(Seq.tabulate(cores)(core => cluster(core).io.source)).sFindFirst(_.ready)

  // Routes cypherblock to designated core
  val toCores = StreamDemux(io.source, toCoresSelection, cores)
  for (core <- 0 until cores)
    toCores(core) <> cluster(core).io.source

  // Checks for first available core to receive the cypherblock
  val (dummy1, toOutsideSelection): (Bool, UInt) = Vec(Seq.tabulate(cores)(core => cluster(core).io.destination)).sFindFirst(_.valid)
  
  // Routes core's results outside of the cluster
  val toOutside = StreamMux(toOutsideSelection, Seq.tabulate(cores)(c => cluster(c).io.destination))
  io.destination <> toOutside
}
