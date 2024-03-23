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
case class AESBlock(payload_width: Int = 128, key_width: Int = 128, encrypts: Boolean = true) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(BlockInterface(payload_width, key_width)))
    val destination = master(Stream(BlockInterface(payload_width, key_width)))
  }
 
  val subBytes    = SubBytes(payload_width, key_width, encrypts)
  val shiftRows   = ShiftRows(payload_width, key_width, encrypts)
  val mixColumns  = MixColumns(payload_width, key_width, encrypts)
  val addRoundKey = AddRoundKey(payload_width, key_width, encrypts)

  if (encrypts) {
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
  else { // decrypts
    // Converts BlockInterface into StageInterface (i.e., UInt to Vec[UInt])
    shiftRows.io.source.valid           := io.source.valid
    shiftRows.io.source.payload.message := io.source.payload.message.subdivideIn(8 bits)
    shiftRows.io.source.payload.key     := io.source.payload.key.subdivideIn(8 bits)
    shiftRows.io.source.payload.round   := io.source.payload.round
    io.source.ready                     := shiftRows.io.source.ready

    // Converts StageInterface into BlockInterface (i.e., Vec[UInt] to UInt)
    io.destination.valid            := mixColumns.io.destination.valid
    io.destination.payload.message  := mixColumns.io.destination.payload.message.asBits.asUInt
    io.destination.payload.key      := mixColumns.io.destination.payload.key.asBits.asUInt
    io.destination.payload.round    := mixColumns.io.destination.payload.round
    mixColumns.io.destination.ready := io.destination.ready

    // Connect pipeline
    shiftRows.io.destination   >> subBytes.io.source
    subBytes.io.destination    >> addRoundKey.io.source
    addRoundKey.io.destination >> mixColumns.io.source
  }
}

object AESEncryptionBlockVerilog extends App {
  Config.spinal.generateVerilog(AESBlock(128, 128, true))
}

object AESEncryptionBlockVhdl extends App {
  Config.spinal.generateVhdl(AESBlock(128, 128, true))
}

object AESDecryptionBlockVerilog extends App {
  Config.spinal.generateVerilog(AESBlock(128, 128, false))
}

object AESDecryptionBlockVhdl extends App {
  Config.spinal.generateVhdl(AESBlock(128, 128, false))
}
