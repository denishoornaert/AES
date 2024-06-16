package aes

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class CoreInterfaceIn(message_width: Int, key_width: Int) extends Bundle {
  val message  = UInt(message_width bits)
  val keys     = Vec.fill(11)(UInt(key_width bits))
}

case class CoreInterfaceOut(message_width: Int) extends Bundle {
  val message  = UInt(message_width bits)
}

case class AESCore(message_width: Int, key_width: Int, encrypts: Boolean = true) extends Component {
  this.setName(if (encrypts) "AESEncryptionCore" else "AESDecryptionCore")

  val io = new Bundle {
    val source      =  slave(Stream(CoreInterfaceIn(message_width, key_width)))
    val destination = master(Stream(CoreInterfaceOut(message_width)))
  }

  val destinationInterface = Reg(master(Flow(CoreInterfaceOut(message_width))))

  io.destination.valid := destinationInterface.valid
  io.destination.payload.message := destinationInterface.message

  val block = AESBlock(message_width, key_width, encrypts)

  val fsm = new StateMachine {
    val counter = Counter(0 to 10)
    val blockInterface = Reg(slave(Flow(BlockInterface(message_width, key_width))))
    block.io.source.valid           := blockInterface.valid
    block.io.source.payload.message := blockInterface.payload.message
    block.io.source.payload.key     := blockInterface.payload.key
    block.io.source.payload.round   := blockInterface.payload.round

    val idle : State = new State with EntryPoint {
      onEntry {
        destinationInterface.valid   := False
        destinationInterface.message := 0
        counter := 1
      }
      whenIsActive {
        when (io.source.fire) {
          val round = if(encrypts) 0 else 10
          val index = if(encrypts) round+1 else round-1
          blockInterface.valid           := True
          blockInterface.payload.message := io.source.payload.message^io.source.payload.keys(round)
          blockInterface.payload.key     := io.source.payload.keys(index)
          blockInterface.payload.round   := index
          goto(encrypt)
        }
      }
    }
    val encrypt : State = new State {
      onEntry {
      }
      whenIsActive {
        val round = if(encrypts) counter.value else 10-counter.value
        val index = if(encrypts) round+1 else round-1
        blockInterface.payload.message := block.io.destination.payload.message
        blockInterface.payload.key     := io.source.payload.keys(index)
        blockInterface.payload.round   := index // round
        when (counter.willOverflowIfInc) {
          blockInterface.valid := False
          goto(ready)
        }
        .otherwise {
          blockInterface.valid := True
          counter.increment()
        }
      }
      onExit {
        destinationInterface.valid := True
        destinationInterface.message := block.io.destination.payload.message
      }
    }
    val ready : State = new State {
      whenIsActive {
        when (io.destination.fire) {
          counter                        := 0
          blockInterface.valid           := False
          blockInterface.payload.message := 0
          blockInterface.payload.key     := 0
          blockInterface.round           := 0
          goto(idle)
        }
      }
    }
  }
  
  io.source.ready            := block.io.source.ready & fsm.isActive(fsm.idle)
  block.io.destination.ready := fsm.isActive(fsm.idle) | fsm.isActive(fsm.encrypt)

}
