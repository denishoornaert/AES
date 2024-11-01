package aes

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class AESCoreWithFifo(message_width: Int, key_width: Int, metadata_width: Int, encrypts: Boolean = true, threads: Int = 4) extends Component {
  this.setName(if (encrypts) "AESEncryptionCore" else "AESDecryptionCore")

  val io = new Bundle {
    val source      =  slave(Stream(CoreInterfaceIn(message_width, key_width, metadata_width)))
    val destination = master(Stream(CoreInterfaceOut(message_width, metadata_width)))
  }
  
  // Core
  val core = AESCore(message_width, key_width, metadata_width, encrypts, threads)

  // Core occupancy counter
  val in_core_messages = Reg(UInt(log2Up(threads)+1 bits)) init(0)
  when (core.io.source.fire && core.io.destination.fire) {
    in_core_messages := in_core_messages
  }
  .elsewhen(core.io.source.fire) {
    in_core_messages := in_core_messages+1
  }
  .elsewhen(core.io.destination.fire) {
    in_core_messages := in_core_messages-1
  }
  .otherwise {
    in_core_messages := in_core_messages
  }

  // Buffer inputs
  val sourceQ = StreamFifo(
    dataType = CoreInterfaceIn(message_width, key_width, metadata_width),
    depth    = threads
  )
  sourceQ.io.pop >> core.io.source
  // The ready is modified to accept only the amount of free space in the core
  sourceQ.io.push.valid   := io.source.valid && ((sourceQ.io.occupancy+^in_core_messages) < threads)
  sourceQ.io.push.payload := io.source.payload
  io.source.ready         := sourceQ.io.push.ready && ((sourceQ.io.occupancy+^in_core_messages) < threads)

  // Buffer outputs
  val destinationQ = StreamFifo(
    dataType = CoreInterfaceOut(message_width, metadata_width),
    depth    = threads
  )
  destinationQ.io.push << core.io.destination
  io.destination << destinationQ.io.pop

}
