package aes

import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._

case class BlockInterface(payload_width: Int, key_width: Int) extends Bundle {
  val message  = UInt(payload_width bits)
  val key      = UInt(key_width bits)
  val round    = UInt(log2Up(10) bits)
}

case class StageInterface(payload_width: Int, key_width: Int) extends Bundle {
  val message  = Vec.fill(payload_width/8)(UInt(8 bits))
  val key      = Vec.fill(key_width/8)(UInt(8 bits))
  val round    = UInt(log2Up(10) bits)
  val constant = UInt(8 bits)
}

// Hardware definition
case class AESBlock(payload_width: Int = 128, key_width: Int = 128, encrypts: Boolean = true, dual_ported: Boolean = false) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(BlockInterface(payload_width, key_width)))
    val destination = master(Stream(BlockInterface(payload_width, key_width)))
  }
 
  val nodes      = Array.fill(4)(Node())
  val stages     = Array.tabulate(3)(n => StageLink(nodes(n), nodes(n+1)))
  val interfaces = Array.fill(3)(Payload(StageInterface(payload_width, key_width)))

  // stage 0
  val subBytes      = SubBytes(payload_width, key_width, encrypts, dual_ported)
  val roundConstant = RoundConstant(encrypts)
  // stage 1  
  val shiftRows     = ShiftRows(payload_width, key_width, encrypts)
  val keySchedule   = KeySchedule(key_width, dual_ported)
  // stage 2
  val mixColumns    = MixColumns(payload_width, key_width, encrypts)
  // stage 3
  val addRoundKey   = AddRoundKey(payload_width, key_width, encrypts)

  if (encrypts) {
    //subBytes.io.source.valid           := io.source.valid
    nodes(0).valid                     := io.source.valid
    subBytes.io.source.payload.message := (io.source.payload.message^Mux(io.source.payload.round === 0, io.source.payload.key, 0)).subdivideIn(8 bits)
    subBytes.io.source.payload.key     := io.source.payload.key.subdivideIn(8 bits)
    subBytes.io.source.payload.round   := io.source.payload.round
    io.source.ready                    := nodes(0).ready

    roundConstant.io.source.payload    := io.source.payload.round

    //subBytes.io.source.payload      := nodes(1)(interfaces(0))
    nodes(0)(interfaces(0)).message    := subBytes.io.destination.payload.message
    nodes(0)(interfaces(0)).key        := subBytes.io.destination.payload.key
    nodes(0)(interfaces(0)).round      := subBytes.io.destination.payload.round
    nodes(0)(interfaces(0)).constant   := roundConstant.io.destination.payload

    shiftRows.io.source.payload        := nodes(1)(interfaces(0))
    keySchedule.io.source.payload.key  := nodes(1)(interfaces(0)).key
    keySchedule.io.source.payload.constant := nodes(1)(interfaces(0)).constant
    nodes(1)(interfaces(1)).message    := shiftRows.io.destination.payload.message
    nodes(1)(interfaces(1)).round      := shiftRows.io.destination.payload.round
    nodes(1)(interfaces(1)).key        := keySchedule.io.destination.payload.key
    nodes(1)(interfaces(1)).constant   := 0
    
    mixColumns.io.source.payload       := nodes(2)(interfaces(1))
    nodes(2)(interfaces(2))            := mixColumns.io.destination.payload
    
    addRoundKey.io.source.valid        := nodes(3).valid
    addRoundKey.io.source.payload      := nodes(3)(interfaces(2))
    nodes(3).ready                     := addRoundKey.io.source.ready
    
    io.destination.valid               := addRoundKey.io.destination.valid
    io.destination.payload.message     := addRoundKey.io.destination.payload.message.asBits.asUInt
    io.destination.payload.key         := addRoundKey.io.destination.payload.key.asBits.asUInt
    io.destination.payload.round       := addRoundKey.io.destination.payload.round
    addRoundKey.io.destination.ready   := io.destination.ready
    
//    addRoundKey.io.source.payload   := nodes(3)(interfaces(2))
//    nodes(3)(interfaces(3))         := addRoundKey.io.destination.payload
//    
//    io.destination.valid           := nodes(4).valid
//    io.destination.payload.message := nodes(4)(interfaces(3)).message.asBits.asUInt
//    io.destination.payload.key     := nodes(4)(interfaces(3)).key.asBits.asUInt
//    io.destination.payload.round   := nodes(4)(interfaces(3)).round
//    nodes(4).ready                 := io.destination.ready
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

  Builder(stages)
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
