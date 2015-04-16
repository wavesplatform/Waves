package scorex.block

import akka.actor.Actor
import settings.Constants

class BlockGenerator extends Actor {

  import scorex.block.BlockGenerator._

  override def receive = {
    case TryToGenerateBlock =>
      val blockchainController = sender()

       Constants.ConsensusAlgo.consensusFunctions.generateBlock().foreach { block =>
        blockchainController ! NewBlock(block, None)
      }
  }
}


object BlockGenerator {
  case object TryToGenerateBlock
}
