package aes

import spinal.core._
import spinal.lib._

import steps._

case class AESInputPort(payload_width: Int, key_width: Int, amount_of_rounds: Int) extends Bundle {
  val round    = UInt(log2Up(amount_of_rounds) bits)
  val message  = UInt(payload_width bits)
  val key      = UInt(key_width bits)
  val constant = UInt(8 bits)
}

case class AESOutputPort(payload_width: Int, key_width: Int) extends Bundle {
  val message  = UInt(payload_width bits)
  val key      = UInt(key_width bits)
  val constant = UInt(8 bits)
}

case class StageInterface(payload_width: Int, key_width: Int) extends Bundle {
  val message  = Vec.fill(payload_width/8)(UInt(8 bits))
  val key      = Vec.fill(key_width/8)(UInt(8 bits))
  val constant = UInt(8 bits)
  val round    = UInt(log2Up(10) bits)
}

// Hardware definition
case class AESEncryptionBlock(payload_width: Int = 128, key_width: Int = 128) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(AESInputPort (payload_width, key_width, 10))) // TODO: for 128 is it 11 or 10 rounds?
    val destination = master(Stream(AESOutputPort(payload_width, key_width)))
  }
  
  val roundConstant = RoundConstant()
  val keySchedule   = KeySchedule(key_width)
  val subBytes      = SubBytes(payload_width)
  val shiftRows     = ShiftRows(payload_width)
  val mixColumns    = MixColumns(payload_width)
  val addRoundKey   = AddRoundKey(payload_width, key_width)

  // create payload sized data for each step of a stage
  val stages  = Array.fill(5)(Stream(StageInterface(payload_width, key_width)))

  // Stage 0
  stages(0).valid            := io.source.valid
  stages(0).payload.message  := io.source.payload.message.subdivideIn(8 bits)
  stages(0).payload.key      := io.source.payload.key.subdivideIn(8 bits)
  stages(0).payload.constant.clearAll()
  stages(0).payload.round    := io.source.payload.round
  io.source.ready := stages(0).ready

  roundConstant.io.source.valid := stages(0).valid
  roundConstant.io.source.payload := stages(0).round
  
  subBytes.io.source.valid := stages(0).valid
  subBytes.io.source.payload := Mux(stages(0).payload.round === U"4'h0", stages(0).payload.message^stages(0).payload.key, stages(0).payload.message)
  
  stages(0).ready := roundConstant.io.source.ready & subBytes.io.source.ready

  // Stage 1
  stages(1).valid                    := stages(0).valid
  stages(1).payload.message          := subBytes.io.destination.payload
  stages(1).payload.key              := stages(0).payload.key
  stages(1).payload.constant         := roundConstant.io.destination.payload
  stages(1).payload.round            := stages(0).payload.round
  roundConstant.io.destination.ready := stages(1).ready
  subBytes.io.destination.ready      := stages(1).ready
  
  keySchedule.io.source.valid := stages(1).valid
  keySchedule.io.source.payload.key := stages(1).payload.key
  keySchedule.io.source.payload.constant := stages(1).payload.constant

  shiftRows.io.source.valid := stages(1).valid
  shiftRows.io.source.payload := stages(1).payload.message

  stages(1).ready := keySchedule.io.source.ready & shiftRows.io.source.ready

  // Stage 2
  stages(2).valid                    := stages(1).valid
  stages(2).payload.message          := shiftRows.io.destination.payload
  stages(2).payload.key              := keySchedule.io.destination.payload.key
  stages(2).payload.constant         := stages(1).payload.constant
  stages(2).payload.round            := stages(1).payload.round
  keySchedule.io.destination.ready   := stages(2).ready
  shiftRows.io.destination.ready     := stages(2).ready

  mixColumns.io.source.valid := stages(2).valid
  mixColumns.io.source.payload := stages(2).payload.message

  stages(2).ready := mixColumns.io.source.ready

  // Stage 3
  stages(3).valid                    := stages(2).valid
  stages(3).payload.message          := Mux(stages(2).payload.round === U"4'h9", stages(2).payload.message, mixColumns.io.destination.payload)
  stages(3).payload.key              := stages(2).payload.key
  stages(3).payload.constant         := stages(2).payload.constant
  stages(3).payload.round            := stages(2).payload.round
  mixColumns.io.destination.ready    := stages(3).ready

  addRoundKey.io.source.valid := stages(3).valid
  addRoundKey.io.source.payload.message := stages(3).payload.message
  addRoundKey.io.source.payload.key := stages(3).payload.key

  stages(3).ready := stages(4).ready

  // Stage 4
  stages(4).valid                    := stages(3).valid
  stages(4).payload.message          := addRoundKey.io.destination.payload.message
  stages(4).payload.key              := stages(3).payload.key
  stages(4).payload.constant         := stages(3).payload.constant
  stages(4).payload.round            := stages(3).payload.round

  stages(4).ready := io.destination.ready

  // assign IO
  io.destination.valid            := stages(4).valid
  io.destination.payload.message  := stages(4).payload.message.asBits.asUInt
  io.destination.payload.constant := stages(4).payload.constant.asBits.asUInt // TODO: not needed I think...
  io.destination.payload.key      := stages(4).payload.key.asBits.asUInt

}

object AESEncryptionBlockVerilog extends App {
  Config.spinal.generateVerilog(AESEncryptionBlock(128))
}

object AESEncryptionBlockVhdl extends App {
  Config.spinal.generateVhdl(AESEncryptionBlock(128))
}
