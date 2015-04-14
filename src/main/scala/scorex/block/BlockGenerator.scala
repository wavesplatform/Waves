package scorex.block

import akka.actor.Actor
import scorex.consensus.qora.QoraBlockGenerationFunctions

class BlockGenerator extends Actor {

  import scorex.block.BlockGenerator._

  override def receive = {
    case TryToGenerateBlock =>
      val blockchainController = sender()

      QoraBlockGenerationFunctions.generateBlock().foreach { block =>
        blockchainController ! NewBlock(block, None)
      }
  }
}


object BlockGenerator {
  case object TryToGenerateBlock
}
