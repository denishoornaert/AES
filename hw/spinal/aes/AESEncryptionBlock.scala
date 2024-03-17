package aes

import spinal.core._
import spinal.lib._

case class BlockInterface(payload_width: Int, key_width: Int) extends Bundle {
  val message  = UInt(payload_width bits)
  val key      = UInt(key_width bits)
  val round    = UInt(log2Up(10) bits)
}

case class StageInterface(payload_width: Int, key_width: Int) extends Bundle {
  val message  = Vec.fill(payload_width/8)(UInt(8 bits))
  val key      = Vec.fill(key_width/8)(UInt(8 bits))
  val round    = UInt(log2Up(10) bits)
}

// Hardware definition
case class AESEncryptionBlock(payload_width: Int = 128, key_width: Int = 128) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(BlockInterface(payload_width, key_width)))
    val destination = master(Stream(BlockInterface(payload_width, key_width)))
  }
 
  val subBytes    = SubBytes(payload_width, key_width)
  val shiftRows   = ShiftRows(payload_width, key_width)
  val mixColumns  = MixColumns(payload_width, key_width)
  val addRoundKey = AddRoundKey(payload_width, key_width)

  // Converts BlockInterface into StageInterface (i.e., UInt to Vec[UInt])
  subBytes.io.source.valid           := io.source.valid
  subBytes.io.source.payload.message := io.source.payload.message.subdivideIn(8 bits)
  subBytes.io.source.payload.key     := io.source.payload.key.subdivideIn(8 bits)
  subBytes.io.source.payload.round   := io.source.payload.round
  io.source.ready                    := subBytes.io.source.ready

  // Converts StageInterface into BlockInterface (i.e., Vec[UInt] to UInt)
  io.destination.valid             := addRoundKey.io.destination.valid
  io.destination.payload.message   := addRoundKey.io.destination.payload.message.asBits.asUInt
  io.destination.payload.key       := addRoundKey.io.destination.payload.key.asBits.asUInt
  io.destination.payload.round     := addRoundKey.io.destination.payload.round
  addRoundKey.io.destination.ready := io.destination.ready

  // Connect pipeline
  subBytes.io.destination    >> shiftRows.io.source
  shiftRows.io.destination   >> mixColumns.io.source
  mixColumns.io.destination  >> addRoundKey.io.source

}

object AESEncryptionBlockVerilog extends App {
  Config.spinal.generateVerilog(AESEncryptionBlock(128))
}

object AESEncryptionBlockVhdl extends App {
  Config.spinal.generateVhdl(AESEncryptionBlock(128))
}
