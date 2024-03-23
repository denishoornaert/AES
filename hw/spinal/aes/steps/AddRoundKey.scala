package aes

import spinal.core._
import spinal.lib._


case class AddRoundKey(message_width: Int, key_width: Int, encrypts: Boolean = true) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(StageInterface(message_width, key_width)))
    val destination = master(Stream(StageInterface(message_width, key_width)))
  }

  io.destination.valid         := io.source.valid
  io.destination.payload.key   := io.source.payload.key
  io.destination.payload.round := io.source.payload.round
  io.source.ready              := io.destination.ready

  for (i <- 0 until message_width/8) {
    io.destination.payload.message(i) := io.source.payload.message(i)^io.source.payload.key(i)
  }
}
