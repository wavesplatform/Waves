package scorex.consensus.mining

import akka.actor._
import scorex.app.Application
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.utils.{NTP, ScorexLogging}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class BlockGeneratorController(application: Application) extends Actor with ScorexLogging {

  import Application.GetBlockGenerationStatus
  import BlockGeneratorController._
  import Miner.{GuessABlock, Stop}

  private var miner: Option[ActorRef] = None

  context.system.scheduler.schedule(SelfCheckInterval, SelfCheckInterval, self, SelfCheck)

  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  override def receive: Receive = idle

  def idle: Receive = state {

    case GetBlockGenerationStatus => sender() ! Idle.name

    case StartGeneration =>
      log.info("Start block generation")
      context.become(generating())
      self ! SelfCheck

    case StopGeneration =>

    case SelfCheck => stopMiner()

    case ConnectedPeers(_) =>

    case LastBlockChanged =>
      if (ifShouldGenerateNow) {
        self ! StartGeneration
      }
  }

  def generating(active: Boolean = true): Receive = state {

    case GetBlockGenerationStatus => sender() ! (if (active) Generating else Suspended).name

    case StartGeneration => if (active) self ! SelfCheck

    case SelfCheck => askForConnectedPeers()

    case StopGeneration =>
      log.info(s"Stop block generation")
      context.become(idle)
      stopMiner()

    case ConnectedPeers(peers) => changeStateAccordingTo(peers.size, active)

    case LastBlockChanged =>
      if (active) {
        if (ifShouldGenerateNow) {
          log.info(s"Enforce miner to generate block")
          miner.foreach { _ ! GuessABlock(rescheduleImmediately = true) }
        } else {
          self ! StopGeneration
        }
      }
  }

  private def ifShouldGenerateNow: Boolean = isLastBlockTsInAllowedToGenerationInterval || isLastBlockIsGenesis

  private def isLastBlockTsInAllowedToGenerationInterval: Boolean = {
    val lastBlockTimestamp = application.history.lastBlock.timestampField.value
    val currentTime = NTP.correctedTime()
    val doNotGenerateUntilLastBlockTs = currentTime - application.settings.allowedGenerationTimeFromLastBlockInterval.toMillis
    lastBlockTimestamp >= doNotGenerateUntilLastBlockTs
  }

  private def isLastBlockIsGenesis: Boolean = application.history.height() == 1

  private def changeStateAccordingTo(peersNumber: Int, active: Boolean): Unit =
    if (peersNumber >= application.settings.quorum || application.settings.offlineGeneration) {
      startMiner()
      if (!active) {
        log.info(s"Resume block generation")
        context become generating(active = true)
      }
    } else {
      stopMiner()
      if (active) {
        log.info(s"Suspend block generation")
        context become generating(active = false)
      }
    }

  private def startMiner() = {
    log.info(s"Check miner")
    miner = Some(createMiner)
    miner.foreach { _ ! GuessABlock(false) }
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
    context.watch(context.actorOf(Props(classOf[Miner], application), "miner"))
  }

  private def askForConnectedPeers() = application.peerManager ! GetConnectedPeersTyped
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

  private val SelfCheckInterval = 5 seconds
}
