package aes

import spinal.core._
import spinal.lib._

import aes.common._

case class SubBytes(message_width: Int, key_width: Int) extends Component {

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

  val io = new Bundle {
    val source      =  slave(Stream(StageInterface(message_width, key_width)))
    val destination = master(Stream(StageInterface(message_width, key_width)))
  }

  io.destination.valid         := io.source.valid
  io.destination.payload.key   := io.source.payload.key
  io.destination.payload.round := io.source.payload.round
  io.source.ready              := io.destination.ready

  for (i <- 0 until message_width/8) {
    val sbox = Mem(UInt(8 bits), initialContent=enc_sbox)
    io.destination.payload.message(i) := sbox.readAsync(io.source.payload.message(i))
  }
}
