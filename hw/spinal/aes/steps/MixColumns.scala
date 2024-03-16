package aes.steps

import spinal.core._
import spinal.lib._

case class MixColumns(message_width: Int) extends Component {

  def x2(encoding: UInt): UInt = {
    return (encoding<<1).resized ^ Mux(encoding.msb, U"8'h1B", U"8'h00")
  }

  def x3(encoding: UInt): UInt = {
    return x2(encoding) ^ encoding
  }

  val io = new Bundle {
    val source      =  slave(Stream(Vec.fill(message_width/8)(UInt(8 bits))))
    val destination = master(Stream(Vec.fill(message_width/8)(UInt(8 bits))))
  }

  io.destination.valid := io.source.valid
  io.source.ready := io.destination.ready

  io.destination.payload( 0) := (x3(io.source.payload( 3)) ^    io.source.payload( 2)  ^    io.source.payload( 1)  ^ x2(io.source.payload( 0))).resized
  io.destination.payload( 1) := (   io.source.payload( 3)  ^    io.source.payload( 2)  ^ x2(io.source.payload( 1)) ^ x3(io.source.payload( 0))).resized
  io.destination.payload( 2) := (   io.source.payload( 3)  ^ x2(io.source.payload( 2)) ^ x3(io.source.payload( 1)) ^    io.source.payload( 0) ).resized
  io.destination.payload( 3) := (x2(io.source.payload( 3)) ^ x3(io.source.payload( 2)) ^    io.source.payload( 1)  ^    io.source.payload( 0) ).resized
  io.destination.payload( 4) := (x3(io.source.payload( 7)) ^    io.source.payload( 6)  ^    io.source.payload( 5)  ^ x2(io.source.payload( 4))).resized
  io.destination.payload( 5) := (   io.source.payload( 7)  ^    io.source.payload( 6)  ^ x2(io.source.payload( 5)) ^ x3(io.source.payload( 4))).resized
  io.destination.payload( 6) := (   io.source.payload( 7)  ^ x2(io.source.payload( 6)) ^ x3(io.source.payload( 5)) ^    io.source.payload( 4) ).resized
  io.destination.payload( 7) := (x2(io.source.payload( 7)) ^ x3(io.source.payload( 6)) ^    io.source.payload( 5)  ^    io.source.payload( 4) ).resized
  io.destination.payload( 8) := (x3(io.source.payload(11)) ^    io.source.payload(10)  ^    io.source.payload( 9)  ^ x2(io.source.payload( 8))).resized
  io.destination.payload( 9) := (   io.source.payload(11)  ^    io.source.payload(10)  ^ x2(io.source.payload( 9)) ^ x3(io.source.payload( 8))).resized
  io.destination.payload(10) := (   io.source.payload(11)  ^ x2(io.source.payload(10)) ^ x3(io.source.payload( 9)) ^    io.source.payload( 8) ).resized
  io.destination.payload(11) := (x2(io.source.payload(11)) ^ x3(io.source.payload(10)) ^    io.source.payload( 9)  ^    io.source.payload( 8) ).resized
  io.destination.payload(12) := (x3(io.source.payload(15)) ^    io.source.payload(14)  ^    io.source.payload(13)  ^ x2(io.source.payload(12))).resized
  io.destination.payload(13) := (   io.source.payload(15)  ^    io.source.payload(14)  ^ x2(io.source.payload(13)) ^ x3(io.source.payload(12))).resized
  io.destination.payload(14) := (   io.source.payload(15)  ^ x2(io.source.payload(14)) ^ x3(io.source.payload(13)) ^    io.source.payload(12) ).resized
  io.destination.payload(15) := (x2(io.source.payload(15)) ^ x3(io.source.payload(14)) ^    io.source.payload(13)  ^    io.source.payload(12) ).resized

}
