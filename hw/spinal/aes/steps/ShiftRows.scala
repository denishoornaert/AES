package aes

import spinal.core._
import spinal.lib._

case class ShiftRows(message_width: Int, key_width: Int, encrypts: Boolean = true) extends Component {

  // Collection of permutations: (from, to)
  val enc_permutations = Array(
    Array( 0, 12), Array( 1,  9), Array( 2,  6), Array( 3,  3),
    Array( 4,  0), Array( 5, 13), Array( 6, 10), Array( 7,  7),
    Array( 8,  4), Array( 9,  1), Array(10, 14), Array(11, 11),
    Array(12,  8), Array(13,  5), Array(14,  2), Array(15, 15)
  )

  // Collection of permutations: (from, to)
  val dec_permutations = Array(
    Array( 0,  4), Array( 1,  9), Array( 2, 14), Array( 3,  3),
    Array( 4,  8), Array( 5, 13), Array( 6,  2), Array( 7,  7),
    Array( 8, 12), Array( 9,  1), Array(10,  6), Array(11, 11),
    Array(12,  0), Array(13,  5), Array(14, 10), Array(15, 15)
  )

  val permutations = if(encrypts) enc_permutations else dec_permutations

  val io = new Bundle {
    val source      =  slave(Stream(StageInterface(message_width, key_width)))
    val destination = master(Stream(StageInterface(message_width, key_width)))
  }

  io.destination.valid         := io.source.valid
  io.destination.payload.key   := io.source.payload.key
  io.destination.payload.round := io.source.payload.round
  io.source.ready              := io.destination.ready

  for (permutation <- permutations) {
    io.destination.payload.message(permutation(1)) := io.source.payload.message(permutation(0))
  }
}
