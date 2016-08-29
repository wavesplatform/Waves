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

  context.system.scheduler.schedule(Duration.Zero, SelfCheckInterval, self, SelfCheck)

  override def receive: Receive = idle

  def idle: Receive = state(Idle) {
    case StartGeneration =>
      log.info("Start block generation")
      context.become(generating)
      self ! SelfCheck

    case SelfCheck =>
      askForConnectedPeers()
      log.info(s"Check miners: $workers vs ${context.children}")
      workers.foreach(w => w ! Stop)
      context.children.foreach(w => w ! Stop)

    case GuessABlock(_) =>
  }

  def generating: Receive = state(Generating) {
    case StopGeneration =>
      log.info(s"Stop block generation")
      context.become(idle)
      self ! SelfCheck

    case SelfCheck =>
      askForConnectedPeers()
      log.info(s"Check ${workers.size} miners")
      val threads = application.settings.miningThreads
      if (threads - workers.size > 0) workers = workers ++ newWorkers(threads - workers.size)
      workers.foreach { _ ! GuessABlock(false) }

    case blockGenerationRequest @ GuessABlock(_) =>
      log.info(s"Enforce miners to generate block: $workers")
      workers.foreach(w => w ! blockGenerationRequest)
  }

  private def state(status: Status)(logic: Receive): Receive =
    logic orElse {
      case ConnectedPeers(peers) =>
        if (peers.size >= application.settings.quorum || application.settings.offlineGeneration) {
          self ! StartGeneration
        } else {
          self ! StopGeneration
        }

      case Terminated(worker) =>
        log.info(s"Miner terminated $worker")
        workers = workers.filter(w => w != worker)

      case GetStatus => sender() ! status.name

      case StartGeneration =>
      case StopGeneration =>

      case m => log.info(s"Unhandled $m in $status")
    }

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

  private case object SelfCheck

  private val SelfCheckInterval = 5 seconds
}
