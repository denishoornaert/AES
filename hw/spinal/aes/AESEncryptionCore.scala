package aes

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class AESCoreInputPort(message_width: Int, key_width: Int) extends Bundle {
  val message  = UInt(message_width bits)
  val key      = UInt(key_width bits)
}

case class AESCoreOutputPort(message_width: Int) extends Bundle {
  val message  = UInt(message_width bits)
}

case class AESEncryptionCore(message_width: Int, key_width: Int) extends Component {
  val io = new Bundle {
    val source      =  slave(Stream(AESCoreInputPort(message_width, key_width)))
    val destination = master(Stream(AESCoreOutputPort(message_width)))
  }

  val staged_destination_valid = Reg(Bool()) init(False)
  val staged_destination = Reg(AESCoreOutputPort(message_width))

  io.destination.valid := staged_destination_valid
  io.destination.payload.message := staged_destination.message

  val block = AESEncryptionBlock(message_width, key_width)
  val blockInput_valid = Reg(Bool())
  val blockInput = Reg(AESInputPort(message_width, key_width, 10))

  block.io.source.valid            := blockInput_valid
  block.io.source.payload.message  := blockInput.message
  block.io.source.payload.key      := blockInput.key
  block.io.source.payload.round    := blockInput.round
  block.io.source.payload.constant := 0

  val fsm = new StateMachine {
    val counter = Counter(0 until 10)

    val idle : State = new State with EntryPoint {
      onEntry {
        staged_destination_valid := False
        staged_destination.message := 0
      }
      whenIsActive {
        when (io.source.fire) {
          blockInput_valid   := True
          blockInput.message := io.source.message
          blockInput.key     := io.source.key
          blockInput.round   := 0
          goto(encrypt)
        }
      }
    }
    val encrypt : State = new State {
      onEntry {
      }
      whenIsActive {
        blockInput.message := block.io.destination.payload.message
        blockInput.key     := block.io.destination.payload.key
        blockInput.round   := counter.valueNext
        when (counter.willOverflowIfInc) {
          blockInput_valid := False
          goto(ready)
        }
        .otherwise {
          blockInput_valid := True
          counter.increment()
        }
      }
      onExit {
        staged_destination_valid := True
        staged_destination.message := block.io.destination.payload.message
      }
    }
    val ready : State = new State {
      whenIsActive {
        when (io.destination.fire) {
          counter := 0
          blockInput_valid   := False
          blockInput.message := 0
          blockInput.key     := 0
          blockInput.round   := 0
          goto(idle)
        }
      }
    }
  }
  
  io.source.ready            := block.io.source.ready & fsm.isActive(fsm.idle)
  block.io.destination.ready := fsm.isActive(fsm.idle) | fsm.isActive(fsm.encrypt)

}
