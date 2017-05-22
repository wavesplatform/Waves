package scorex.network

import akka.actor.{Actor, ActorRef}
import com.wavesplatform.network.Network
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.language.{higherKinds, postfixOps}

class Coordinator(network: Network, blockchainSynchronizer: ActorRef, blockGenerator: ActorRef,
                  peerManager: ActorRef, scoreObserver: ActorRef, blockchainUpdater: BlockchainUpdater, time: Time,
                  utxStorage: UnconfirmedTransactionsStorage,
                  history: History, stateReader: StateReader, checkpoints: CheckpointService, settings: WavesSettings) extends Actor with ScorexLogging {



  override def receive: Receive = { case _ => }

  private def handleCheckpoint(checkpoint: Checkpoint, from: Option[ConnectedPeer]): Unit =
    if (checkpoints.get.forall(c => !(c.signature sameElements checkpoint.signature))) {
      val maybePublicKeyBytes = Base58.decode(settings.checkpointsSettings.publicKey).toOption

      maybePublicKeyBytes foreach {
        publicKey =>
          if (EllipticCurveImpl.verify(checkpoint.signature, checkpoint.toSign, publicKey)) {
            checkpoints.set(Some(checkpoint))
//            network.broadcast(checkpoint, from.map(_ => ???)) // todo: don't broadcast to sender
            makeBlockchainCompliantWith(checkpoint)
          } else {
            from.foreach(_.blacklist())
          }
      }
    }

  private def makeBlockchainCompliantWith(checkpoint: Checkpoint): Unit = {
    val existingItems = checkpoint.items.filter {
      checkpoint => history.blockAt(checkpoint.height).isDefined
    }

    val fork = existingItems.takeWhile {
      case BlockCheckpoint(h, sig) =>
        val block = history.blockAt(h).get
        block.signerData.signature != ByteStr(sig)
    }

    if (fork.nonEmpty) {
      val genesisBlockHeight = 1
      val hh = existingItems.map(_.height) :+ genesisBlockHeight
      history.blockAt(hh(fork.size)).foreach {
        lastValidBlock =>
          log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block id [${lastValidBlock.encodedId}]")
          blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }


  def str(b: Block) = ""
}

object Coordinator extends ScorexLogging {

  case class AddBlock(block: Block, generator: Option[ConnectedPeer])

  case class BroadcastCheckpoint(checkpoint: Checkpoint)
}
