package aes.steps

import spinal.core._
import spinal.lib._

case class RoundConstant() extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(UInt(log2Up(10) bits)))
    val destination = master(Stream(UInt(8 bits)))
  }

  io.destination.valid := io.source.valid
  io.source.ready := io.destination.ready
  
  io.destination.payload := ((B"80'x361B8040201008040201".subdivideIn(8 bits))(io.source.payload)).asUInt
}
