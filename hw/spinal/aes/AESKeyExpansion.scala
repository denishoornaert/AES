package aes

import spinal.core._
import spinal.lib._


case class KeyExpansionInputPort(key_width: Int) extends Bundle {
  val key   = UInt(key_width bits)
  val round = UInt(log2Up(10) bits)
}

case class KeyExpansionOutputPort(key_width: Int) extends Bundle {
  val key   = UInt(key_width bits)
}

case class KeyExpansion(key_width: Int, encrypts: Boolean = true) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(KeyExpansionInputPort(key_width)))
    val destination = master(Stream(KeyExpansionOutputPort(key_width)))
  }

  val roundConstant = RoundConstant(encrypts)
  val keySchedule   = KeySchedule(key_width)

  io.source.ready := roundConstant.io.source.ready

  roundConstant.io.source.valid := io.source.valid
  roundConstant.io.source.payload := io.source.payload.round
  roundConstant.io.destination.ready := keySchedule.io.destination.ready

  keySchedule.io.source.valid := roundConstant.io.source.valid
  keySchedule.io.source.payload.key := io.source.payload.key.subdivideIn(8 bits)
  keySchedule.io.source.payload.constant := roundConstant.io.destination.payload
  keySchedule.io.destination.ready := io.destination.ready

  io.destination.valid := keySchedule.io.destination.valid
  io.destination.payload.key := keySchedule.io.destination.payload.key.asBits.asUInt

}
