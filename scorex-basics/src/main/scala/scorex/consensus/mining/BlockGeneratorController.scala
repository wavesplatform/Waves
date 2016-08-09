package scorex.consensus.mining

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scorex.app.RunnableApplication
import scorex.consensus.mining.BlockGeneratorController._
import scorex.consensus.mining.Miner._
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Success

class BlockGeneratorController(application: RunnableApplication) extends Actor with ScorexLogging {

  val threads = application.settings.mininigThreads
  val FailedGenerationDelay: FiniteDuration = Math.max(10, application.settings.blockGenerationDelay.toSeconds).seconds
  implicit val timeout = Timeout(FailedGenerationDelay)

  var workers: Seq[ActorRef] = Seq.empty

  context.system.scheduler.schedule(FailedGenerationDelay, FailedGenerationDelay, self, CheckWorkers)


  override def receive: Receive = syncing

  def syncing: Receive = {
    case StartGeneration =>
      log.info("Start block generation")
      workers.foreach(w => w ! Miner.GuessABlock)
      context.become(generating)

    case GetStatus =>
      sender() ! Syncing.name

    case CheckWorkers =>
      log.info(s"Check miners: $workers vs ${context.children}")
      workers.foreach(w => w ! Stop)
      context.children.foreach(w => w ! Stop)

    case StopGeneration =>

    case m => log.warn(s"Unhandled $m in Syncing")
  }

  def generating: Receive = {
    case StopGeneration =>
      log.info(s"Stop block generation")
      workers.foreach(w => w ! Stop)
      context.become(syncing)

    case Terminated(worker) =>
      log.info(s"Miner terminated $worker")
      workers = workers.filter(w => w != worker)

    case GetStatus =>
      sender() ! Generating.name

    case CheckWorkers =>
      log.info(s"Check ${workers.size} miners")
      val t = System.currentTimeMillis()
      workers.foreach { worker =>
        (worker ? HealthCheck) onComplete {
          case Success(HealthOk) =>
            val responseTime = System.currentTimeMillis() - t
            if (responseTime < FailedGenerationDelay.toMillis) {
              log.trace(s"Miner $worker works fine, last try was $responseTime millis ago")
            } else {
              log.warn(s"Miner $worker responds slowly ($responseTime millis)")
              worker ! Miner.GuessABlock
            }
          case m =>
            log.warn(s"Restart miner $worker: $m")
            worker ! Stop
        }
      }
      if (threads - workers.size > 0) workers = workers ++ newWorkers(threads - workers.size)

    case Miner.GuessABlock =>
      log.info(s"Enforce miners to generate block: $workers")
      workers.foreach(w => w ! Miner.GuessABlock)

    case m => log.info(s"Unhandled $m in Generating")
  }

  def newWorkers(count: Int): Seq[ActorRef] = (1 to count).map { i =>
    context.watch(context.actorOf(Props(classOf[Miner], application), s"Worker-${System.currentTimeMillis()}-$i"))
  }
}

object BlockGeneratorController {

  sealed trait Status {
    val name: String
  }

  case object Syncing extends Status {
    override val name = "syncing"
  }

  case object Generating extends Status {
    override val name = "generating"
  }

  case object GetStatus

  case object StartGeneration

  case object StopGeneration

  case object CheckWorkers
}
