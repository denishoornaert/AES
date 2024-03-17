package aes

import spinal.core._
import spinal.lib._

case class MixColumns(message_width: Int, key_width: Int, encrypts: Boolean = true) extends Component {

  def x2(encoding: UInt): UInt = {
    return (encoding<<1).resized ^ Mux(encoding.msb, U"8'h1B", U"8'h00")
  }

  def x3(encoding: UInt): UInt = {
    return x2(encoding) ^ encoding
  }

  val io = new Bundle {
    val source      =  slave(Stream(StageInterface(message_width, key_width)))
    val destination = master(Stream(StageInterface(message_width, key_width)))
  }

  io.destination.valid         := io.source.valid
  io.destination.payload.key   := io.source.payload.key
  io.destination.payload.round := io.source.payload.round
  io.source.ready              := io.destination.ready
  
  when (io.source.payload.round =/= U"4'h9") {
    io.destination.payload.message( 0) := (x3(io.source.payload.message( 3)) ^    io.source.payload.message( 2)  ^    io.source.payload.message( 1)  ^ x2(io.source.payload.message( 0))).resized
    io.destination.payload.message( 1) := (   io.source.payload.message( 3)  ^    io.source.payload.message( 2)  ^ x2(io.source.payload.message( 1)) ^ x3(io.source.payload.message( 0))).resized
    io.destination.payload.message( 2) := (   io.source.payload.message( 3)  ^ x2(io.source.payload.message( 2)) ^ x3(io.source.payload.message( 1)) ^    io.source.payload.message( 0) ).resized
    io.destination.payload.message( 3) := (x2(io.source.payload.message( 3)) ^ x3(io.source.payload.message( 2)) ^    io.source.payload.message( 1)  ^    io.source.payload.message( 0) ).resized
    io.destination.payload.message( 4) := (x3(io.source.payload.message( 7)) ^    io.source.payload.message( 6)  ^    io.source.payload.message( 5)  ^ x2(io.source.payload.message( 4))).resized
    io.destination.payload.message( 5) := (   io.source.payload.message( 7)  ^    io.source.payload.message( 6)  ^ x2(io.source.payload.message( 5)) ^ x3(io.source.payload.message( 4))).resized
    io.destination.payload.message( 6) := (   io.source.payload.message( 7)  ^ x2(io.source.payload.message( 6)) ^ x3(io.source.payload.message( 5)) ^    io.source.payload.message( 4) ).resized
    io.destination.payload.message( 7) := (x2(io.source.payload.message( 7)) ^ x3(io.source.payload.message( 6)) ^    io.source.payload.message( 5)  ^    io.source.payload.message( 4) ).resized
    io.destination.payload.message( 8) := (x3(io.source.payload.message(11)) ^    io.source.payload.message(10)  ^    io.source.payload.message( 9)  ^ x2(io.source.payload.message( 8))).resized
    io.destination.payload.message( 9) := (   io.source.payload.message(11)  ^    io.source.payload.message(10)  ^ x2(io.source.payload.message( 9)) ^ x3(io.source.payload.message( 8))).resized
    io.destination.payload.message(10) := (   io.source.payload.message(11)  ^ x2(io.source.payload.message(10)) ^ x3(io.source.payload.message( 9)) ^    io.source.payload.message( 8) ).resized
    io.destination.payload.message(11) := (x2(io.source.payload.message(11)) ^ x3(io.source.payload.message(10)) ^    io.source.payload.message( 9)  ^    io.source.payload.message( 8) ).resized
    io.destination.payload.message(12) := (x3(io.source.payload.message(15)) ^    io.source.payload.message(14)  ^    io.source.payload.message(13)  ^ x2(io.source.payload.message(12))).resized
    io.destination.payload.message(13) := (   io.source.payload.message(15)  ^    io.source.payload.message(14)  ^ x2(io.source.payload.message(13)) ^ x3(io.source.payload.message(12))).resized
    io.destination.payload.message(14) := (   io.source.payload.message(15)  ^ x2(io.source.payload.message(14)) ^ x3(io.source.payload.message(13)) ^    io.source.payload.message(12) ).resized
    io.destination.payload.message(15) := (x2(io.source.payload.message(15)) ^ x3(io.source.payload.message(14)) ^    io.source.payload.message(13)  ^    io.source.payload.message(12) ).resized
  }
  .otherwise {
    io.destination.payload.message := io.source.payload.message
  }

}
