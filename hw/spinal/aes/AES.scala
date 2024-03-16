package aes

import spinal.core._
import spinal.lib._

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

// Hardware definition
case class AES(payload_width: Int = 128, key_width: Int = 128) extends Component {
  val io = new Bundle {
/*
    val key               = in    (        UInt(key_width bits)     )
    val round             = in    (        UInt(log2Up(11) bits)    )
    val previous_constant = in    (        UInt(8 bits)             )
    val roundkey          = out   (        UInt(key_width bits)     )
    val constant          = out   (        UInt(8 bits)             )
    val source            = slave ( Stream(UInt(payload_width bits)))
    val destination       = master( Stream(UInt(payload_width bits)))
*/
    val source      =  slave(Stream(AESInputPort (payload_width, key_width, 11))) // TODO: for 128 is it 11 or 10 rounds?
    val destination = master(Stream(AESOutputPort(payload_width, key_width)))
  }

  // Collection of permutations: (from, to)
  val permutations = Array(
    Array( 0, 12), Array( 1,  9), Array( 2,  6), Array( 3,  3),
    Array( 4,  0), Array( 5, 13), Array( 6, 10), Array( 7,  7),
    Array( 8,  4), Array( 9,  1), Array(10, 14), Array(11, 11),
    Array(12,  8), Array(13,  5), Array(14,  2), Array(15, 15)
  )

  val enc_sbox = Seq[UInt](
    U"8'h63", U"8'h7c", U"8'h77", U"8'h7b", U"8'hf2", U"8'h6b", U"8'h6f", U"8'hc5", U"8'h30", U"8'h01", U"8'h67", U"8'h2b", U"8'hfe", U"8'hd7", U"8'hab", U"8'h76",
    U"8'hca", U"8'h82", U"8'hc9", U"8'h7d", U"8'hfa", U"8'h59", U"8'h47", U"8'hf0", U"8'had", U"8'hd4", U"8'ha2", U"8'haf", U"8'h9c", U"8'ha4", U"8'h72", U"8'hc0",
    U"8'hb7", U"8'hfd", U"8'h93", U"8'h26", U"8'h36", U"8'h3f", U"8'hf7", U"8'hcc", U"8'h34", U"8'ha5", U"8'he5", U"8'hf1", U"8'h71", U"8'hd8", U"8'h31", U"8'h15",
    U"8'h04", U"8'hc7", U"8'h23", U"8'hc3", U"8'h18", U"8'h96", U"8'h05", U"8'h9a", U"8'h07", U"8'h12", U"8'h80", U"8'he2", U"8'heb", U"8'h27", U"8'hb2", U"8'h75",
    U"8'h09", U"8'h83", U"8'h2c", U"8'h1a", U"8'h1b", U"8'h6e", U"8'h5a", U"8'ha0", U"8'h52", U"8'h3b", U"8'hd6", U"8'hb3", U"8'h29", U"8'he3", U"8'h2f", U"8'h84",
    U"8'h53", U"8'hd1", U"8'h00", U"8'hed", U"8'h20", U"8'hfc", U"8'hb1", U"8'h5b", U"8'h6a", U"8'hcb", U"8'hbe", U"8'h39", U"8'h4a", U"8'h4c", U"8'h58", U"8'hcf",
    U"8'hd0", U"8'hef", U"8'haa", U"8'hfb", U"8'h43", U"8'h4d", U"8'h33", U"8'h85", U"8'h45", U"8'hf9", U"8'h02", U"8'h7f", U"8'h50", U"8'h3c", U"8'h9f", U"8'ha8",
    U"8'h51", U"8'ha3", U"8'h40", U"8'h8f", U"8'h92", U"8'h9d", U"8'h38", U"8'hf5", U"8'hbc", U"8'hb6", U"8'hda", U"8'h21", U"8'h10", U"8'hff", U"8'hf3", U"8'hd2",
    U"8'hcd", U"8'h0c", U"8'h13", U"8'hec", U"8'h5f", U"8'h97", U"8'h44", U"8'h17", U"8'hc4", U"8'ha7", U"8'h7e", U"8'h3d", U"8'h64", U"8'h5d", U"8'h19", U"8'h73",
    U"8'h60", U"8'h81", U"8'h4f", U"8'hdc", U"8'h22", U"8'h2a", U"8'h90", U"8'h88", U"8'h46", U"8'hee", U"8'hb8", U"8'h14", U"8'hde", U"8'h5e", U"8'h0b", U"8'hdb",
    U"8'he0", U"8'h32", U"8'h3a", U"8'h0a", U"8'h49", U"8'h06", U"8'h24", U"8'h5c", U"8'hc2", U"8'hd3", U"8'hac", U"8'h62", U"8'h91", U"8'h95", U"8'he4", U"8'h79",
    U"8'he7", U"8'hc8", U"8'h37", U"8'h6d", U"8'h8d", U"8'hd5", U"8'h4e", U"8'ha9", U"8'h6c", U"8'h56", U"8'hf4", U"8'hea", U"8'h65", U"8'h7a", U"8'hae", U"8'h08",
    U"8'hba", U"8'h78", U"8'h25", U"8'h2e", U"8'h1c", U"8'ha6", U"8'hb4", U"8'hc6", U"8'he8", U"8'hdd", U"8'h74", U"8'h1f", U"8'h4b", U"8'hbd", U"8'h8b", U"8'h8a",
    U"8'h70", U"8'h3e", U"8'hb5", U"8'h66", U"8'h48", U"8'h03", U"8'hf6", U"8'h0e", U"8'h61", U"8'h35", U"8'h57", U"8'hb9", U"8'h86", U"8'hc1", U"8'h1d", U"8'h9e",
    U"8'he1", U"8'hf8", U"8'h98", U"8'h11", U"8'h69", U"8'hd9", U"8'h8e", U"8'h94", U"8'h9b", U"8'h1e", U"8'h87", U"8'he9", U"8'hce", U"8'h55", U"8'h28", U"8'hdf",
    U"8'h8c", U"8'ha1", U"8'h89", U"8'h0d", U"8'hbf", U"8'he6", U"8'h42", U"8'h68", U"8'h41", U"8'h99", U"8'h2d", U"8'h0f", U"8'hb0", U"8'h54", U"8'hbb", U"8'h16"
  )

  val dec_sbox = Array[UInt](
    U(0x52, 8 bits), U(0x09, 8 bits), U(0x6a, 8 bits), U(0xd5, 8 bits), U(0x30, 8 bits), U(0x36, 8 bits), U(0xa5, 8 bits), U(0x38, 8 bits), U(0xbf, 8 bits), U(0x40, 8 bits), U(0xa3, 8 bits), U(0x9e, 8 bits), U(0x81, 8 bits), U(0xf3, 8 bits), U(0xd7, 8 bits), U(0xfb, 8 bits),
    U(0x7c, 8 bits), U(0xe3, 8 bits), U(0x39, 8 bits), U(0x82, 8 bits), U(0x9b, 8 bits), U(0x2f, 8 bits), U(0xff, 8 bits), U(0x87, 8 bits), U(0x34, 8 bits), U(0x8e, 8 bits), U(0x43, 8 bits), U(0x44, 8 bits), U(0xc4, 8 bits), U(0xde, 8 bits), U(0xe9, 8 bits), U(0xcb, 8 bits),
    U(0x54, 8 bits), U(0x7b, 8 bits), U(0x94, 8 bits), U(0x32, 8 bits), U(0xa6, 8 bits), U(0xc2, 8 bits), U(0x23, 8 bits), U(0x3d, 8 bits), U(0xee, 8 bits), U(0x4c, 8 bits), U(0x95, 8 bits), U(0x0b, 8 bits), U(0x42, 8 bits), U(0xfa, 8 bits), U(0xc3, 8 bits), U(0x4e, 8 bits),
    U(0x08, 8 bits), U(0x2e, 8 bits), U(0xa1, 8 bits), U(0x66, 8 bits), U(0x28, 8 bits), U(0xd9, 8 bits), U(0x24, 8 bits), U(0xb2, 8 bits), U(0x76, 8 bits), U(0x5b, 8 bits), U(0xa2, 8 bits), U(0x49, 8 bits), U(0x6d, 8 bits), U(0x8b, 8 bits), U(0xd1, 8 bits), U(0x25, 8 bits),
    U(0x72, 8 bits), U(0xf8, 8 bits), U(0xf6, 8 bits), U(0x64, 8 bits), U(0x86, 8 bits), U(0x68, 8 bits), U(0x98, 8 bits), U(0x16, 8 bits), U(0xd4, 8 bits), U(0xa4, 8 bits), U(0x5c, 8 bits), U(0xcc, 8 bits), U(0x5d, 8 bits), U(0x65, 8 bits), U(0xb6, 8 bits), U(0x92, 8 bits),
    U(0x6c, 8 bits), U(0x70, 8 bits), U(0x48, 8 bits), U(0x50, 8 bits), U(0xfd, 8 bits), U(0xed, 8 bits), U(0xb9, 8 bits), U(0xda, 8 bits), U(0x5e, 8 bits), U(0x15, 8 bits), U(0x46, 8 bits), U(0x57, 8 bits), U(0xa7, 8 bits), U(0x8d, 8 bits), U(0x9d, 8 bits), U(0x84, 8 bits),
    U(0x90, 8 bits), U(0xd8, 8 bits), U(0xab, 8 bits), U(0x00, 8 bits), U(0x8c, 8 bits), U(0xbc, 8 bits), U(0xd3, 8 bits), U(0x0a, 8 bits), U(0xf7, 8 bits), U(0xe4, 8 bits), U(0x58, 8 bits), U(0x05, 8 bits), U(0xb8, 8 bits), U(0xb3, 8 bits), U(0x45, 8 bits), U(0x06, 8 bits),
    U(0xd0, 8 bits), U(0x2c, 8 bits), U(0x1e, 8 bits), U(0x8f, 8 bits), U(0xca, 8 bits), U(0x3f, 8 bits), U(0x0f, 8 bits), U(0x02, 8 bits), U(0xc1, 8 bits), U(0xaf, 8 bits), U(0xbd, 8 bits), U(0x03, 8 bits), U(0x01, 8 bits), U(0x13, 8 bits), U(0x8a, 8 bits), U(0x6b, 8 bits),
    U(0x3a, 8 bits), U(0x91, 8 bits), U(0x11, 8 bits), U(0x41, 8 bits), U(0x4f, 8 bits), U(0x67, 8 bits), U(0xdc, 8 bits), U(0xea, 8 bits), U(0x97, 8 bits), U(0xf2, 8 bits), U(0xcf, 8 bits), U(0xce, 8 bits), U(0xf0, 8 bits), U(0xb4, 8 bits), U(0xe6, 8 bits), U(0x73, 8 bits),
    U(0x96, 8 bits), U(0xac, 8 bits), U(0x74, 8 bits), U(0x22, 8 bits), U(0xe7, 8 bits), U(0xad, 8 bits), U(0x35, 8 bits), U(0x85, 8 bits), U(0xe2, 8 bits), U(0xf9, 8 bits), U(0x37, 8 bits), U(0xe8, 8 bits), U(0x1c, 8 bits), U(0x75, 8 bits), U(0xdf, 8 bits), U(0x6e, 8 bits),
    U(0x47, 8 bits), U(0xf1, 8 bits), U(0x1a, 8 bits), U(0x71, 8 bits), U(0x1d, 8 bits), U(0x29, 8 bits), U(0xc5, 8 bits), U(0x89, 8 bits), U(0x6f, 8 bits), U(0xb7, 8 bits), U(0x62, 8 bits), U(0x0e, 8 bits), U(0xaa, 8 bits), U(0x18, 8 bits), U(0xbe, 8 bits), U(0x1b, 8 bits),
    U(0xfc, 8 bits), U(0x56, 8 bits), U(0x3e, 8 bits), U(0x4b, 8 bits), U(0xc6, 8 bits), U(0xd2, 8 bits), U(0x79, 8 bits), U(0x20, 8 bits), U(0x9a, 8 bits), U(0xdb, 8 bits), U(0xc0, 8 bits), U(0xfe, 8 bits), U(0x78, 8 bits), U(0xcd, 8 bits), U(0x5a, 8 bits), U(0xf4, 8 bits),
    U(0x1f, 8 bits), U(0xdd, 8 bits), U(0xa8, 8 bits), U(0x33, 8 bits), U(0x88, 8 bits), U(0x07, 8 bits), U(0xc7, 8 bits), U(0x31, 8 bits), U(0xb1, 8 bits), U(0x12, 8 bits), U(0x10, 8 bits), U(0x59, 8 bits), U(0x27, 8 bits), U(0x80, 8 bits), U(0xec, 8 bits), U(0x5f, 8 bits),
    U(0x60, 8 bits), U(0x51, 8 bits), U(0x7f, 8 bits), U(0xa9, 8 bits), U(0x19, 8 bits), U(0xb5, 8 bits), U(0x4a, 8 bits), U(0x0d, 8 bits), U(0x2d, 8 bits), U(0xe5, 8 bits), U(0x7a, 8 bits), U(0x9f, 8 bits), U(0x93, 8 bits), U(0xc9, 8 bits), U(0x9c, 8 bits), U(0xef, 8 bits),
    U(0xa0, 8 bits), U(0xe0, 8 bits), U(0x3b, 8 bits), U(0x4d, 8 bits), U(0xae, 8 bits), U(0x2a, 8 bits), U(0xf5, 8 bits), U(0xb0, 8 bits), U(0xc8, 8 bits), U(0xeb, 8 bits), U(0xbb, 8 bits), U(0x3c, 8 bits), U(0x83, 8 bits), U(0x53, 8 bits), U(0x99, 8 bits), U(0x61, 8 bits),
    U(0x17, 8 bits), U(0x2b, 8 bits), U(0x04, 8 bits), U(0x7e, 8 bits), U(0xba, 8 bits), U(0x77, 8 bits), U(0xd6, 8 bits), U(0x26, 8 bits), U(0xe1, 8 bits), U(0x69, 8 bits), U(0x14, 8 bits), U(0x63, 8 bits), U(0x55, 8 bits), U(0x21, 8 bits), U(0x0c, 8 bits), U(0x7d, 8 bits)
  )

  val matrix = Array(
    Array(2, 3, 1, 1),
    Array(1, 2, 3, 1),
    Array(1, 1, 2, 3),
    Array(3, 1, 1, 2)
  )

  def GenerateRoundConstant(round: UInt, prev: UInt): UInt = {
    return ((B"80'x361B8040201008040201".subdivideIn(8 bits))(round)).asUInt
  }

  // Specialized for AES-128 must be augmented for other versions!
  def KeySchedule(key: Vec[UInt], constant: UInt): Vec[UInt] = {
    // Reshape key
    val key_by_32 = key.asBits.subdivideIn(32 bits)
    // To result
    val round_key_by_32 = Vec.fill(widthOf(key)/32)(Bits(32 bits))
    // Handle word 3
    val modified_word_3 = Bits(32 bits)
    val sub_bytes = Vec.fill(widthOf(modified_word_3)/8)(UInt(8 bits))
    for (i <- 0 until widthOf(modified_word_3)/8) {
      val sbox = Mem(UInt(8 bits), initialContent=enc_sbox)
      sub_bytes(i) := sbox.readAsync(key_by_32(0)(i*8 until (i+1)*8).asUInt)
    }
    modified_word_3 := Cat(sub_bytes(2)^constant, sub_bytes(1), sub_bytes(0), sub_bytes(3))
    // Handle word 0
    round_key_by_32(3) := key_by_32(3)^modified_word_3
    // Handle word 1
    round_key_by_32(2) := round_key_by_32(3)^key_by_32(2)
    // Handle word 2
    round_key_by_32(1) := round_key_by_32(2)^key_by_32(1)
    // Handle word 3
    round_key_by_32(0) := round_key_by_32(1)^key_by_32(0)
    // Result
    val res = round_key_by_32.asBits.subdivideIn(8 bits)
    // Cast to vec of UInts
    val res_as_UInt = Vec.fill(widthOf(res)/8)(UInt(8 bits))
    for (i <- 0 until widthOf(res)/8) {
      res_as_UInt(i) := res(i).asUInt
    }
    return res_as_UInt
  }

  def SubBytes(payload: Vec[UInt]): Vec[UInt] = {
    val bytes = Vec.fill(widthOf(payload)/8)(UInt(8 bits))
    for (i <- 0 until widthOf(payload)/8) {
      val sbox = Mem(UInt(8 bits), initialContent=enc_sbox)
      bytes(i) := sbox.readAsync(payload(i))
    }
    return bytes
  }

  def ShiftRows(payload: Vec[UInt]): Vec[UInt] = {
    val bytes = Vec.fill(payload.getBitsWidth/8)(UInt(8 bits))
    for (permutation <- permutations) {
      bytes(permutation(1)) := payload(permutation(0))
    }
    return bytes
  }

  def MixColumns(payload: Vec[UInt]): Vec[UInt] = {
    def x2(encoding: UInt): UInt = {
      return (encoding<<1).resized ^ Mux(encoding.msb, U"8'h1B", U"8'h00")
    }

    def x3(encoding: UInt): UInt = {
      return x2(encoding) ^ encoding
    }

    val bytes = Vec.fill(payload.getBitsWidth/8)(UInt(8 bits))
    bytes( 0) := (x3(payload( 3)) ^    payload( 2)  ^    payload( 1)  ^ x2(payload( 0))).resized
    bytes( 1) := (   payload( 3)  ^    payload( 2)  ^ x2(payload( 1)) ^ x3(payload( 0))).resized
    bytes( 2) := (   payload( 3)  ^ x2(payload( 2)) ^ x3(payload( 1)) ^    payload( 0) ).resized
    bytes( 3) := (x2(payload( 3)) ^ x3(payload( 2)) ^    payload( 1)  ^    payload( 0) ).resized
    bytes( 4) := (x3(payload( 7)) ^    payload( 6)  ^    payload( 5)  ^ x2(payload( 4))).resized
    bytes( 5) := (   payload( 7)  ^    payload( 6)  ^ x2(payload( 5)) ^ x3(payload( 4))).resized
    bytes( 6) := (   payload( 7)  ^ x2(payload( 6)) ^ x3(payload( 5)) ^    payload( 4) ).resized
    bytes( 7) := (x2(payload( 7)) ^ x3(payload( 6)) ^    payload( 5)  ^    payload( 4) ).resized
    bytes( 8) := (x3(payload(11)) ^    payload(10)  ^    payload( 9)  ^ x2(payload( 8))).resized
    bytes( 9) := (   payload(11)  ^    payload(10)  ^ x2(payload( 9)) ^ x3(payload( 8))).resized
    bytes(10) := (   payload(11)  ^ x2(payload(10)) ^ x3(payload( 9)) ^    payload( 8) ).resized
    bytes(11) := (x2(payload(11)) ^ x3(payload(10)) ^    payload( 9)  ^    payload( 8) ).resized
    bytes(12) := (x3(payload(15)) ^    payload(14)  ^    payload(13)  ^ x2(payload(12))).resized
    bytes(13) := (   payload(15)  ^    payload(14)  ^ x2(payload(13)) ^ x3(payload(12))).resized
    bytes(14) := (   payload(15)  ^ x2(payload(14)) ^ x3(payload(13)) ^    payload(12) ).resized
    bytes(15) := (x2(payload(15)) ^ x3(payload(14)) ^    payload(13)  ^    payload(12) ).resized
    return bytes
  }

  def AddRoundKey(payload: Vec[UInt], subkey: Vec[UInt]): Vec[UInt] = {
    val bytes = Vec.fill(payload.getBitsWidth/8)(UInt(8 bits))
    for (i <- 0 until payload.getBitsWidth/8) {
      bytes(i) := payload(i)^subkey(i)
    }
    return bytes
  }

  // decompose input in bytes
  val payload           = io.source.payload.message.subdivideIn(8 bits)
  val key               = io.source.payload.key.subdivideIn(8 bits)
  val round             = io.source.payload.round
  val previous_constant = io.source.payload.constant

  // create payload sized data for each step of a stage
  val steps    = Array.fill(4)(Vec.fill(widthOf(payload)/8)(UInt(8 bits)))
  val constant = UInt(8 bits)
  val roundkey = Vec.fill(key.getBitsWidth/8)(UInt(8 bits))

  // whenever source handshake happens, perform stage with payload
  when (io.source.valid && io.source.ready) {
    constant := this.GenerateRoundConstant(round, previous_constant)
    roundkey := this.KeySchedule(key, constant)
    steps(0) := this.SubBytes(Mux(round === U"4'h0", payload^key, payload))
    steps(1) := this.ShiftRows(steps(0))
    steps(2) := Mux(round === U"4'h9", steps(1), this.MixColumns(steps(1)))
    steps(3) := this.AddRoundKey(steps(2), roundkey)
  }
  .otherwise {
    constant := U"8'h00"
    roundkey := Vec.fill(widthOf(payload)/8)(U(0, 8 bits))
    steps(0) := Vec.fill(widthOf(payload)/8)(U(0, 8 bits))
    steps(1) := Vec.fill(widthOf(payload)/8)(U(1, 8 bits))
    steps(2) := Vec.fill(widthOf(payload)/8)(U(2, 8 bits))
    steps(3) := Vec.fill(widthOf(payload)/8)(U(3, 8 bits))
  }

  // assign IO
  io.source.ready                 := io.destination.ready
  io.destination.valid            := io.source.valid
  io.destination.payload.message  := steps(3).asBits.asUInt
  io.destination.payload.constant := constant.asBits.asUInt
  io.destination.payload.key      := roundkey.asBits.asUInt

}

object AESVerilog extends App {
  Config.spinal.generateVerilog(AES(128))
}

object AESVhdl extends App {
  Config.spinal.generateVhdl(AES(128))
}
