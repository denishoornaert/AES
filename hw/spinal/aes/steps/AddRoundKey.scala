package aes.steps

import spinal.core._
import spinal.lib._

case class AddRoundKeyInputPort(message_width: Int, key_width: Int) extends Bundle {
  val message = Vec.fill(message_width/8)(UInt(8 bits))
  val key     = Vec.fill(key_width/8)(UInt(8 bits))
}

case class AddRoundKeyOutputPort(message_width: Int) extends Bundle {
  val message = Vec.fill(message_width/8)(UInt(8 bits))
}

case class AddRoundKey(message_width: Int, key_width: Int) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(AddRoundKeyInputPort(message_width, key_width)))
    val destination = master(Stream(AddRoundKeyOutputPort(message_width)))
  }

  io.destination.valid := io.source.valid
  io.source.ready := io.destination.ready

  for (i <- 0 until message_width/8) {
    io.destination.payload.message(i) := io.source.payload.message(i)^io.source.payload.key(i)
  }
}
