package scorex.consensus.mining

import akka.actor._
import com.wavesplatform.settings.{BlockchainSettings, MinerSettings}
import com.wavesplatform.state2.reader.StateReader
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.transaction.{History, UnconfirmedTransactionsStorage}
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class BlockGeneratorController(minerSettings: MinerSettings,
                               history: History,
                               time: Time,
                               peerManager: ActorRef,
                               wallet: Wallet,
                               stateReader: StateReader,
                               blockchainSettings: BlockchainSettings,
                               utxStorage: UnconfirmedTransactionsStorage,
                               coordinator: ActorRef) extends Actor with ScorexLogging {

  import BlockGeneratorController._
  import Miner.{GuessABlock, Stop}

  private var miner: Option[ActorRef] = None

  override def preStart(): Unit = {
    if (minerSettings.enable) {
      context.system.scheduler.schedule(SelfCheckInterval, SelfCheckInterval, self, SelfCheck)
    }
  }

  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  override def receive: Receive = if (minerSettings.enable) idle else Actor.ignoringBehavior

  def idle: Receive = state {

    case GetStatus =>
      sender() ! Idle.name

    case StartGeneration =>
      startGeneratingIfShould()

    case StopGeneration =>

    case SelfCheck =>
      stopMiner()
      startGeneratingIfShould()

    case ConnectedPeers(_) =>

    case LastBlockChanged =>
      self ! StartGeneration
  }

  def generating(active: Boolean = true): Receive = state {

    case GetStatus =>
      sender() ! (if (active) Generating else Suspended).name

    case StartGeneration =>
      if (active) self ! SelfCheck

    case SelfCheck => askForConnectedPeers()

    case StopGeneration =>
      log.info(s"Stop block generation")
      context.become(idle)
      stopMiner()

    case ConnectedPeers(peers) =>
      changeStateAccordingTo(peers.size, active)

    case LastBlockChanged =>
      if (active) {
        if (ifShouldGenerateNow) {
          log.info(s"Enforce miner to generate block")
          miner.foreach {
            _ ! GuessABlock(rescheduleImmediately = true)
          }
        } else {
          self ! StopGeneration
        }
      }
  }

  private def startGeneratingIfShould() = {
    if (ifShouldGenerateNow) {
      log.info("Start block generation")
      context.become(generating())
      self ! SelfCheck
    }
  }

  private def ifShouldGenerateNow: Boolean = isLastBlockTsInAllowedToGenerationInterval || isLastBlockIsGenesis

  private def isLastBlockTsInAllowedToGenerationInterval: Boolean = try {
    val lastBlockTimestamp = history.lastBlock.timestamp
    time.correctedTime() <= lastBlockTimestamp + minerSettings.intervalAfterLastBlockThenGenerationIsAllowed.toMillis
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      false
  }

  private def isLastBlockIsGenesis: Boolean = history.height() == 1

  private def changeStateAccordingTo(peersNumber: Int, active: Boolean): Unit = {
    def suspendGeneration(): Unit = {
      log.info(s"Suspend block generation")
      context become generating(active = false)
    }

    def resumeGeneration(): Unit = {
      log.info(s"Resume block generation")
      context become generating(active = true)
    }

    if (peersNumber >= minerSettings.quorum || minerSettings.offline) {
      if (ifShouldGenerateNow) {
        startMiner()
        if (!active) {
          resumeGeneration()
        }
      } else {
        suspendGeneration()
      }
    } else {
      stopMiner()
      if (active) {
        suspendGeneration()
      }
    }
  }

  private def startMiner() = if (miner.isEmpty) {
    log.info(s"Check miner")
    miner = Some(createMiner)
    miner.foreach {
      _ ! GuessABlock(false)
    }
  }

  private def stopMiner() = if (miner.nonEmpty) {
    log.info(s"Stop miner")
    miner.foreach(_ ! Stop)
    miner = None
  }

  private def state(logic: Receive): Receive =
    logic orElse {
      case Terminated(worker) =>
        log.info(s"Miner terminated $worker")
        miner = None

      case m => log.info(s"Unhandled $m")
    }

  private def createMiner: ActorRef = {
    context.watch(context.actorOf(Props(new Miner(minerSettings,
      wallet,
      history,
      stateReader,
      blockchainSettings,
      utxStorage,
      time,
      coordinator)), "miner"))
  }

  private def askForConnectedPeers() = peerManager ! GetConnectedPeersTyped
}

object BlockGeneratorController {

  sealed trait Status {
    val name: String
  }

  case object Idle extends Status {
    override val name = "idle"
  }

  case object Generating extends Status {
    override val name = "generating"
  }

  case object Suspended extends Status {
    override val name = "suspended"
  }

  case object StartGeneration

  case object StopGeneration

  case object LastBlockChanged

  private[mining] case object SelfCheck

  case object GetStatus

  private val SelfCheckInterval = 5.seconds
}
