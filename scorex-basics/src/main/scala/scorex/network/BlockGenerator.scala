package scorex.network

import java.net.InetSocketAddress

import akka.actor.FSM
import scorex.app.Application
import scorex.block.Block
import scorex.network.BlockGenerator._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


case class NewBlock(block: Block, sender: Option[InetSocketAddress])

class BlockGenerator(application: Application) extends FSM[Status, Unit] {

  startWith(Syncing, Unit)

  when(Syncing) {
    case Event(NewBlock(block, remoteOpt), _) =>
      assert(remoteOpt.isDefined, "Local generation attempt while syncing")
      stay()
  }

  when(Synced) {
    case Event(NewBlock(block, remoteOpt), _) =>
      stay()
  }

  initialize()

  def tryToGenerateABlock(): Unit = {
    implicit val transactionalModule = application.transactionModule

    log.info("Trying to generate a new block")
    val accounts = application.wallet.privateKeyAccounts()
    application.consensusModule.generateNextBlocks(accounts)(application.transactionModule) onComplete {
      case Success(blocks: Seq[Block]) =>
        if (blocks.nonEmpty) {
          val bestBlock = blocks.maxBy(application.consensusModule.blockScore)
          self ! NewBlock(bestBlock, None)
        }
      case Failure(ex) => log.error("Failed to generate new block: {}", ex)
      case m => log.error("Unexpected message: {}", m)
    }
  }
}

object BlockGenerator {

  sealed trait Status {
    val name: String
  }

  case object Syncing extends Status {
    override val name = "syncing"
  }

  case object Synced extends Status {
    override val name = "generating"
  }

  case object GetStatus
}