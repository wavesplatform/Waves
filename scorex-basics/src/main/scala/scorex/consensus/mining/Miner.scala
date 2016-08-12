package scorex.consensus.mining

import akka.actor.{Actor, Cancellable}
import scorex.app.Application
import scorex.block.Block
import scorex.consensus.mining.Miner._
import scorex.network.Coordinator.AddBlock
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Try}

class Miner(application: Application) extends Actor with ScorexLogging {

  import System.currentTimeMillis

  private lazy val blockGenerationDelay =
    math.max(application.settings.blockGenerationDelay.toMillis, BlockGenerationTimeShift.toMillis) millis

  private implicit lazy val transactionModule = application.transactionModule
  private lazy val consensusModule = application.consensusModule

  private var currentState = Option.empty[(Seq[Cancellable], Block)]

  private def accounts = application.wallet.privateKeyAccounts()

  override def receive: Receive = {
    case GuessABlock =>
      val lastBlock = application.history.lastBlock
      if (!currentState.exists(_._2 == lastBlock)) {
        scheduleBlockGeneration(lastBlock)
      }

    case GenerateBlock(repeat) =>
      val notStopped = currentState.nonEmpty

      if (notStopped) {
        tryToGenerateABlock()

        if (repeat) {
          scheduleBlockGeneration(application.history.lastBlock)
        }
      }

    case HealthCheck => sender ! HealthOk

    case Stop => stop()
  }

  private def stop(): Unit = {
    currentState.foreach { case (cancellable, _) =>
      cancellable.foreach(_.cancel())
      currentState = None
    }
  }

  private def tryToGenerateABlock(): Unit = Try {
    log.info("Trying to generate a new block")

    val blocks = application.consensusModule.generateNextBlocks(accounts)
    if (blocks.nonEmpty) {
      application.coordinator ! AddBlock(blocks.max(consensusModule.blockOrdering), None)
    }
  }.recoverWith {
    case ex =>
      log.error(s"Failed to generate new block: ${ex.getMessage}")
      Failure(ex)
  }

  private def scheduleBlockGeneration(lastBlock: Block): Unit = {

    stop()

    val currentTime = currentTimeMillis

    val schedule = if (application.settings.tflikeScheduling) {
      accounts
        .flatMap(acc => consensusModule.nextBlockGenerationTime(lastBlock, acc).map(_ + BlockGenerationTimeShift.toMillis))
        .map(t => math.max(t - currentTime, blockGenerationDelay.toMillis))
        .map(_ millis)
        .distinct.sorted
    } else Seq.empty

    val systemScheduler = context.system.scheduler
    val tasks = if (schedule.isEmpty) {
      log.info(s"Next block generation will start in $blockGenerationDelay")
      Seq(systemScheduler.scheduleOnce(blockGenerationDelay, self, GenerateBlock(true)))
    } else {
      log.info(s"Block generation schedule: ${schedule.take(7).mkString(", ")}...")
      schedule.map { t => systemScheduler.scheduleOnce(t, self, GenerateBlock(t == blockGenerationDelay)) }
    }

    currentState = Some(tasks, lastBlock)
  }
}

object Miner {

  case object Stop

  case object GuessABlock

  case object HealthCheck

  case object HealthOk

  private case class GenerateBlock(repeat: Boolean)

  private[mining] val BlockGenerationTimeShift = 1 second
}
