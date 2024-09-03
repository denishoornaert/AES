package aes

import spinal.core._
import spinal.lib._

case class RoundConstant(encrypts: Boolean = true) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(UInt(log2Up(10) bits)))
    val destination = master(Stream(UInt(8 bits)))
  }

  val pattern = if (encrypts) B"80'x361B8040201008040201" else B"'x01020408102040801B36"

  io.destination.valid := io.source.valid
  io.source.ready := io.destination.ready
  
  io.destination.payload := ((pattern.subdivideIn(8 bits))(io.source.payload)).asUInt
}
