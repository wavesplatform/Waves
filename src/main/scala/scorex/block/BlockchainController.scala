package scorex.block

import java.net.InetSocketAddress
import java.util.logging.Logger

import akka.actor.{Actor, ActorRef, Props}
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.network.NetworkController
import scorex.network.message.{BlockMessage, GetSignaturesMessage}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


case class Synchronize(peer: InetSocketAddress)

case class NewBlock(block: Block, sender: Option[InetSocketAddress])

case class BlocksDownload(signatures: List[Array[Byte]], peer: InetSocketAddress)

case class BlockchainController(networkController: ActorRef) extends Actor {

  import BlockchainController._

  private var status = Status.Offline

  private val blockGenerator = context.actorOf(Props[BlockGenerator])

  override def preStart() = {
    context.system.scheduler.schedule(1.second, 2.seconds)(self ! CheckState)
    context.system.scheduler.schedule(500.millis, 1.second)(networkController ! GetMaxChainScore)
  }

  override def receive = {
    case CheckState =>
      status match {
        case Status.Offline =>

        case Status.Syncing =>
          val msg = GetSignaturesMessage(PrunableBlockchainStorage.lastSignatures())
          networkController ! NetworkController.SendMessageToBestPeer(msg)

        case Status.Generating =>
          blockGenerator ! BlockGenerator.TryToGenerateBlock
      }

    case MaxChainScore(scoreOpt) => scoreOpt match {
      case Some(maxScore) =>
        if (maxScore > PrunableBlockchainStorage.height()) {
          status = Status.Syncing
        } else {
          status = Status.Generating
        }
      case None => status = Status.Offline
    }

    case NewBlock(block, remoteOpt) =>
      if (Block.isNewBlockValid(block)) {
        Logger.getGlobal.info(s"New block: $block")
        block.process()
        PrunableBlockchainStorage.appendBlock(block)
        val height = PrunableBlockchainStorage.height()
        val exceptOf = remoteOpt.map(r => List(r)).getOrElse(List())
        networkController ! NetworkController.BroadcastMessage(BlockMessage(height, block), exceptOf)
      } else {
        Logger.getGlobal.warning(s"Non-valid block: $block from $remoteOpt")
      }

    case GetStatus => sender() ! Status.replicate(status)

    case a: Any => Logger.getGlobal.warning(s"BlockchainController: got something strange $a")
  }
}

object BlockchainController {

  object Status extends Enumeration {
    val Offline = Value(0)
    val Syncing = Value(1)
    val Generating = Value(2)

    def replicate(status: Status.Value) = Value(status.id)
  }

  case object CheckState

  case object GetMaxChainScore

  case class MaxChainScore(scoreOpt: Option[Int])

  case object GetStatus

}