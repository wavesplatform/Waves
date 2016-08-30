package scorex.consensus.mining

import akka.actor._
import scorex.app.Application
import scorex.consensus.mining.BlockGeneratorController._
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class BlockGeneratorController(application: Application) extends Actor with ScorexLogging {

  import Miner.{GuessABlock, Stop}

  private var workers: Seq[ActorRef] = Seq.empty

  context.system.scheduler.schedule(SelfCheckInterval, SelfCheckInterval, self, SelfCheck)

  override def receive: Receive = idle

  def idle: Receive = state(Idle) {
    case StartGeneration =>
      log.info("Start block generation")
      context.become(generating())
      self ! SelfCheck

    case StopGeneration =>

    case SelfCheck => stopWorkers()

    case ConnectedPeers(_) =>

    case GuessABlock(_) =>
  }

  def generating(active: Boolean = true): Receive = state(Generating) {
    case StartGeneration =>
      if (active) self ! SelfCheck

    case SelfCheck =>
      askForConnectedPeers()

    case StopGeneration =>
      log.info(s"Stop block generation")
      context.become(idle)
      stopWorkers()

    case ConnectedPeers(peers) =>
      if (generationAllowed(peers)) {
        log.info(s"Resume block generation")
        startWorkers()
        context become generating(active = true)
      } else {
        log.info(s"Suspend block generation")
        stopWorkers()
        context become generating(active = false)
      }
      startWorkers()
      self ! StopGeneration

    case blockGenerationRequest @ GuessABlock(_) =>
      if (active) {
        log.info(s"Enforce miners to generate block: $workers")
        workers.foreach(_ ! blockGenerationRequest)
      }
  }

  private def startWorkers() = {
    log.info(s"Check ${workers.size} miners")
    val threads = application.settings.miningThreads
    if (threads - workers.size > 0) workers = workers ++ newWorkers(threads - workers.size)
    workers.foreach { _ ! GuessABlock(false) }
  }

  private def stopWorkers() = {
    log.info(s"Stop miners: $workers vs ${context.children}")
    workers.foreach(_ ! Stop)
  }

  private def state(status: Status)(logic: Receive): Receive =
    logic orElse {
      case Terminated(worker) =>
        log.info(s"Miner terminated $worker")
        workers = workers.filter(_ != worker)

      case GetStatus => sender() ! status.name

      case m => log.info(s"Unhandled $m in $status")
    }

  private def generationAllowed(peers: Seq[_]) =
    peers.size >= application.settings.quorum || application.settings.offlineGeneration

  private def newWorkers(count: Int): Seq[ActorRef] =  1 to count map { i =>
    context.watch(context.actorOf(Props(classOf[Miner], application), s"Worker-${System.currentTimeMillis()}-$i"))
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

  case object GetStatus

  case object StartGeneration

  case object StopGeneration

  private[mining] case object SelfCheck

  private val SelfCheckInterval = 5 seconds
}
