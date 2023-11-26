package aes

import spinal.core._

// Hardware definition
case class AES() extends Component {
  val io = new Bundle {
  }

}

object AESVerilog extends App {
  Config.spinal.generateVerilog(AES())
}

object AESVhdl extends App {
  Config.spinal.generateVhdl(AES())
}
