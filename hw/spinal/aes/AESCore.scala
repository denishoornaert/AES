package aes

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._


case class CoreInterfaceIn(message_width: Int, key_width: Int) extends Bundle {
  val message = UInt(message_width bits)
  val key     = UInt(key_width bits)
}

case class CoreInterfaceOut(message_width: Int) extends Bundle {
  val message = UInt(message_width bits)
}

case class AESCore(message_width: Int, key_width: Int, encrypts: Boolean = true, threads: Int = 4) extends Component {
  this.setName(if (encrypts) "AESEncryptionCore" else "AESDecryptionCore")

  val io = new Bundle {
    val source      =  slave(Stream(CoreInterfaceIn(message_width, key_width)))
    val destination = master(Stream(CoreInterfaceOut(message_width)))
  }

  val activeThread = CounterFreeRun(threads)
  val block = AESBlock(message_width, key_width, encrypts)

  val contexts = Vec(Stream(BlockInterface(message_width, key_width)), threads)
  val outproduct = Vec(Stream(CoreInterfaceOut(message_width)), threads)

  val FSMStoBlock = StreamMux(activeThread, contexts)
  block.io.source <> FSMStoBlock

  val BlocktoFSMS = StreamDemux(block.io.destination, activeThread.valueNext, threads)

  val FSMStoDestination = StreamMux(activeThread, outproduct)
  io.destination <> FSMStoDestination

  val fsm = for (threadId <- 0 until threads) yield new StateMachine {
    val counter = Counter(0 to 10)

    val context = Reg(Flow(BlockInterface(message_width, key_width)))
    // Must be explicitly set
    context.valid init(False)
    context.payload.message init(0)
    context.payload.key init(0)
    context.payload.round init(0)

    contexts(threadId).valid := context.valid
    contexts(threadId).payload := context.payload

    BlocktoFSMS(threadId).ready := True // TODO: probably too bold!

    val idle : State = new State with EntryPoint {
      onEntry {
        context.valid  := False
        counter        := 0        
      }
      whenIsActive {
        when (io.source.fire & (activeThread.valueNext === threadId)) {
          val round = if(encrypts) 0 else 10
          val index = if(encrypts) round+1 else round-1
          context.valid           := True
          context.payload.message := io.source.payload.message//^io.source.payload.keys(round)
          context.payload.key     := io.source.payload.key//s(round)//(index)
          context.payload.round   := round
          counter.increment()
          goto(encrypt)
        }
      }
    }
    val encrypt : State = new State {
      whenIsActive {
        val round = if(encrypts) counter.value else 10-counter.value
        val index = if(encrypts) round+1 else round-1
        when (BlocktoFSMS(threadId).fire & counter.willOverflowIfInc & (activeThread.valueNext === threadId)) {
          // Before switching to the 'ready' state, store Block outcome in register 
          context.valid := False
          context.payload.message := BlocktoFSMS(threadId).payload.message
          context.payload.key     := 0
          context.payload.round   := 0
          goto(ready)
        }
        .elsewhen (BlocktoFSMS(threadId).fire & (activeThread.valueNext === threadId)) {
          context.valid := True
          context.payload.message := BlocktoFSMS(threadId).payload.message
          context.payload.key     := BlocktoFSMS(threadId).payload.key//io.source.payload.keys(round)//(index)
          context.payload.round   := counter
          counter.increment()
        }
        .elsewhen (contexts(threadId).fire) {
          context.valid := False
        }
        .otherwise {
          context.valid := context.valid
        }
      }
    }
    val ready : State = new State {
      whenIsActive {
        when (FSMStoDestination.fire & (activeThread === threadId)) {
          goto(idle)
        }
      }
      onExit {
          counter                 := 0
          context.valid           := False
          context.payload.message := 0
          context.payload.key     := 0
          context.round           := 0
      }
    }

    outproduct(threadId).valid := this.isActive(this.ready)
    outproduct(threadId).payload.message := context.message
  }

  io.source.ready := block.io.source.ready & Vec(Seq.tabulate(threads)(t => fsm(t).isActive(fsm(t).idle)))(activeThread.valueNext)

//  io.destination.valid := fsm.result.valid & fsm.isActive(fsm.ready)
//  io.destination.payload.message := fsm.result.message
//  fsm.result.ready := fsm.isActive(fsm.idle) | fsm.isActive(fsm.encrypt)
}
