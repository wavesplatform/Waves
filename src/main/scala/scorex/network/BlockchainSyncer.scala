package scorex.network

import java.net.InetSocketAddress

import akka.actor.FSM
import scorex.app.LagonakiApplication
import scorex.block.Block
import scorex.network.BlockchainSyncer._
import scorex.network.message.{BlockMessage, GetSignaturesMessage}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


case class NewBlock(block: Block, sender: Option[InetSocketAddress])


//todo: reduce boilerplate code
case class BlockchainSyncer(application: LagonakiApplication) extends FSM[Status, Unit] {

  private lazy val networkController = application.networkController

  private val stateTimeout = 1.second

  startWith(Offline, Unit)

  context.system.scheduler.schedule(500.millis, 1.second)(networkController ! GetMaxChainScore)

  when(Offline, stateTimeout) {
    case Event(StateTimeout, _) =>
      stay()

    case Event(Unit, _) =>
      log.info("Initializing")
      stay()
  }

  when(Syncing) {
    case Event(MaxChainScore(scoreOpt), _) => scoreOpt match {
      case Some(maxScore) =>
        val localScore = application.blockchainImpl.score()
        log.info(s"maxScore: $maxScore, localScore: $localScore")
        if (maxScore > localScore) {
          val sigs = application.blockchainImpl.lastSignatures(application.settings.MaxBlocksChunks)
          val msg = GetSignaturesMessage(sigs)
          networkController ! NetworkController.SendMessageToBestPeer(msg)
          stay()
        } else goto(Generating)

      case None =>
        if (application.settings.offlineGeneration) goto(Generating).using(Unit) else goto(Offline)
    }

    case Event(NewBlock(block, remoteOpt), _) =>
      assert(remoteOpt.isDefined, "Local generation attempt while syncing")
      processNewBlock(block, remoteOpt)
      stay()
  }

  when(Generating) {
    case Event(NewBlock(block, remoteOpt), _) =>
      processNewBlock(block, remoteOpt)
      stay()

    case Event(MaxChainScore(scoreOpt), _) => scoreOpt match {
      case Some(maxScore) =>
        val localScore = application.blockchainImpl.score()
        log.info(s"maxScore: $maxScore, localScore: $localScore")
        if (maxScore > localScore) goto(Syncing)
        else {
          tryToGenerateABlock()
          stay()
        }

      case None =>
        tryToGenerateABlock()
        stay()
    }
  }

  //common logic for all the states
  whenUnhandled {
    case Event(MaxChainScore(scoreOpt), _) => scoreOpt match {
      case Some(maxScore) =>
        val localScore = application.blockchainImpl.score()
        log.info(s"maxScore: $maxScore, localScore: $localScore")
        if (maxScore > localScore) goto(Syncing) else goto(Generating)

      case None =>
        if (application.settings.offlineGeneration) goto(Generating).using(Unit) else goto(Offline)
    }

    case Event(GetStatus, _) =>
      sender() ! super.stateName.name
      stay()

    case Event(e, s) =>
      log.warning(s"received unhandled request {$e} in state {$stateName}/{$s}")
      stay()
  }

  initialize()


  def processNewBlock(block: Block, remoteOpt: Option[InetSocketAddress]) = {
    val fromStr = remoteOpt.map(_.toString).getOrElse("local")
    if (block.isValid) {
      log.info(s"New block: $block from $fromStr")
      application.storedState.processBlock(block)
      application.blockchainImpl.appendBlock(block)

      block.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)
      val height = application.blockchainImpl.height()

      //broadcast block only if it is generated locally
      if (remoteOpt.isEmpty) {
        networkController ! NetworkController.BroadcastMessage(BlockMessage(height, block), List())
      }
    } else {
      log.warning(s"Non-valid block: $block from $fromStr")
    }
  }

  def tryToGenerateABlock() = {
    log.info("Trying to generate a new block")
    val appState = application.storedState
    val nonEmptyAccs = application.wallet.privateKeyAccounts().filter(acc => appState.balance(acc.address) > 0)
    nonEmptyAccs.find {
      privKeyAcc =>
        implicit val transactionModule = application.transactionModule

        //As Proof-of-Stake is being used for Scorex Lagonaki, generateNextBlock() finishes quickly
        //  (it should be, at least) so we're just going to wait for a result
        Await.result(application.consensusModule.generateNextBlock(privKeyAcc), 500.millis) match {
          case Some(block) =>
            self ! NewBlock(block, None)
            true
          case None => false
        }
    }
  }
}


object BlockchainSyncer {

  sealed trait Status {
    val name: String
  }

  case object Offline extends Status {
    override val name = "offline"
  }

  case object Syncing extends Status {
    override val name = "syncing"
  }

  case object Generating extends Status {
    override val name = "generating"
  }

  case class MaxChainScore(scoreOpt: Option[BigInt])

  case object GetMaxChainScore

  case object GetStatus

}