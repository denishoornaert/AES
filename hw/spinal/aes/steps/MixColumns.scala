package aes

import spinal.core._
import spinal.lib._

case class MixColumns(message_width: Int, key_width: Int, encrypts: Boolean = true) extends Component {

  // Multiplications can be implmented as lookup tables (pre-computed).
  // Tables can be found here: https://en.wikipedia.org/wiki/Rijndael_MixColumns

  def x2(encoding: UInt): UInt = {
    return (encoding<<1).resized ^ Mux(encoding.msb, U"8'h1B", U"8'h00")
  }

  def x3(encoding: UInt): UInt = {
    return x2(encoding) ^ encoding
  }

  def x9(encoding: UInt): UInt = {
    return x2(x2(x2(encoding))) ^ encoding
  }

  def x11(encoding: UInt): UInt = {
    return x2(x2(x2(encoding)) ^ encoding) ^ encoding
  }

  def x13(encoding: UInt): UInt = {
    return x2(x2(x2(encoding) ^ encoding)) ^ encoding
  }

  def x14(encoding: UInt): UInt = {
    return x2(x2(x2(encoding) ^ encoding) ^ encoding)
  }

  val io = new Bundle {
    val source      =  slave(Stream(StageInterface(message_width, key_width)))
    val destination = master(Stream(StageInterface(message_width, key_width)))
  }

  io.destination.valid         := io.source.valid
  io.destination.payload.key   := io.source.payload.key
  io.destination.payload.round := io.source.payload.round
  io.source.ready              := io.destination.ready
  
  if (encrypts) {
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
  else { // decrypts
    when (io.source.payload.round =/= U"4'h0") {
      io.destination.payload.message( 0) := (x11(io.source.payload.message( 3)) ^ x13(io.source.payload.message( 2)) ^  x9(io.source.payload.message( 1)) ^ x14(io.source.payload.message( 0))).resized
      io.destination.payload.message( 1) := (x13(io.source.payload.message( 3)) ^  x9(io.source.payload.message( 2)) ^ x14(io.source.payload.message( 1)) ^ x11(io.source.payload.message( 0))).resized
      io.destination.payload.message( 2) := ( x9(io.source.payload.message( 3)) ^ x14(io.source.payload.message( 2)) ^ x11(io.source.payload.message( 1)) ^ x13(io.source.payload.message( 0))).resized
      io.destination.payload.message( 3) := (x14(io.source.payload.message( 3)) ^ x11(io.source.payload.message( 2)) ^ x13(io.source.payload.message( 1)) ^  x9(io.source.payload.message( 0))).resized 
      io.destination.payload.message( 4) := (x11(io.source.payload.message( 7)) ^ x13(io.source.payload.message( 6)) ^  x9(io.source.payload.message( 5)) ^ x14(io.source.payload.message( 4))).resized
      io.destination.payload.message( 5) := (x13(io.source.payload.message( 7)) ^  x9(io.source.payload.message( 6)) ^ x14(io.source.payload.message( 5)) ^ x11(io.source.payload.message( 4))).resized
      io.destination.payload.message( 6) := ( x9(io.source.payload.message( 7)) ^ x14(io.source.payload.message( 6)) ^ x11(io.source.payload.message( 5)) ^ x13(io.source.payload.message( 4))).resized
      io.destination.payload.message( 7) := (x14(io.source.payload.message( 7)) ^ x11(io.source.payload.message( 6)) ^ x13(io.source.payload.message( 5)) ^  x9(io.source.payload.message( 4))).resized
      io.destination.payload.message( 8) := (x11(io.source.payload.message(11)) ^ x13(io.source.payload.message(10)) ^  x9(io.source.payload.message( 9)) ^ x14(io.source.payload.message( 8))).resized
      io.destination.payload.message( 9) := (x13(io.source.payload.message(11)) ^  x9(io.source.payload.message(10)) ^ x14(io.source.payload.message( 9)) ^ x11(io.source.payload.message( 8))).resized
      io.destination.payload.message(10) := ( x9(io.source.payload.message(11)) ^ x14(io.source.payload.message(10)) ^ x11(io.source.payload.message( 9)) ^ x13(io.source.payload.message( 8))).resized
      io.destination.payload.message(11) := (x14(io.source.payload.message(11)) ^ x11(io.source.payload.message(10)) ^ x13(io.source.payload.message( 9)) ^  x9(io.source.payload.message( 8))).resized
      io.destination.payload.message(12) := (x11(io.source.payload.message(15)) ^ x13(io.source.payload.message(14)) ^  x9(io.source.payload.message(13)) ^ x14(io.source.payload.message(12))).resized
      io.destination.payload.message(13) := (x13(io.source.payload.message(15)) ^  x9(io.source.payload.message(14)) ^ x14(io.source.payload.message(13)) ^ x11(io.source.payload.message(12))).resized
      io.destination.payload.message(14) := ( x9(io.source.payload.message(15)) ^ x14(io.source.payload.message(14)) ^ x11(io.source.payload.message(13)) ^ x13(io.source.payload.message(12))).resized
      io.destination.payload.message(15) := (x14(io.source.payload.message(15)) ^ x11(io.source.payload.message(14)) ^ x13(io.source.payload.message(13)) ^  x9(io.source.payload.message(12))).resized
    }
    .otherwise {
      io.destination.payload.message := io.source.payload.message
    }
  }

}
