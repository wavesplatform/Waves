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

  private implicit lazy val transactionModule = application.transactionModule
  private lazy val consensusModule = application.consensusModule

  private var currentState = Option.empty[(Seq[Cancellable], Block)]

  private def accounts = application.wallet.privateKeyAccounts()

  self ! GuessABlock

  override def receive: Receive = {
    case GuessABlock =>
      val lastBlock = application.history.lastBlock
      if (!currentState.exists(_._2 == lastBlock)) {
        stop()
        scheduleForging(lastBlock)
      }

    case Forge => tryToGenerateABlock()

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

  private def scheduleForging(lastBlock: Block): Unit = {
    val currentTime = currentTimeMillis

    val blockGenerationDelayInMillis = application.settings.blockGenerationDelay.toMillis

    val calculatedSchedule = accounts
      .flatMap(acc => consensusModule.nextBlockForgingTime(lastBlock, acc).map(_ + ForgingTimeShift.toMillis))
      .map(t => math.max(t - currentTime, blockGenerationDelayInMillis))

    val finalSchedule =
      if (calculatedSchedule.nonEmpty) calculatedSchedule else {
        Seq(blockGenerationDelayInMillis)
      }

    log.debug(s"Block forging schedule in seconds: ${finalSchedule.map(_ / 1000).take(7).mkString(", ")} etc...")

    val tasks = finalSchedule.map { t => context.system.scheduler.scheduleOnce(t millis, self, Forge) }

    currentState = Some(tasks, lastBlock)
  }
}

object Miner {

  case object Stop

  case object GuessABlock

  case object HealthCheck

  case object HealthOk

  private case object Forge

  val ForgingTimeShift = 1 second

}
