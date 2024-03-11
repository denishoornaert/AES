package aes

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._


// Hardware definition
case class AES(payload_width: Int) extends Component{

  val axiConfig = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth      = 16,
    // useId = false,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useQos = false,
    useLen = false,
    useResp = false,
    useSize = false,
    useProt = false,
    useLast = false
  )

 val axi_in = slave(Axi4(axiConfig)) 
 val axi_out = master(Axi4(axiConfig))

 axi_in >> axi_out


}

object AESVerilog extends App {
  Config.spinal.generateVerilog(AES(128))
}

object AESVhdl extends App {
  Config.spinal.generateVhdl(AES(128))
}