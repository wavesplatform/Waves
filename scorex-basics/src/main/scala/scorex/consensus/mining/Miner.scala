package scorex.consensus.mining

import akka.actor.{Actor, Cancellable}
import scorex.app.Application
import scorex.consensus.mining.Miner._
import scorex.network.Coordinator.AddBlock
import scorex.utils.{NTP, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Try}

class Miner(application: Application) extends Actor with ScorexLogging {

  private lazy val blockGenerationDelay =
    math.max(application.settings.blockGenerationDelay.toMillis, BlockGenerationTimeShift.toMillis) millis

  private implicit lazy val transactionModule = application.transactionModule
  private lazy val consensusModule = application.consensusModule

  private var currentState = Option.empty[Seq[Cancellable]]

  private def accounts = application.wallet.privateKeyAccounts()

  override def receive: Receive = {
    case GuessABlock(rescheduleImmediately) =>
      if (rescheduleImmediately) { cancel() }
      if (currentState.isEmpty) { scheduleBlockGeneration() }

    case GenerateBlock =>
      cancel()

      val blockGenerated = tryToGenerateABlock()

      if (!blockGenerated) {
        scheduleBlockGeneration()
      }

    case Stop => cancel()
  }

  private def cancel(): Unit = {
    currentState.toSeq.flatten.foreach(_.cancel())
    currentState = None
  }

  private def tryToGenerateABlock(): Boolean = Try {
    log.info("Trying to generate a new block")

    val blocks = application.consensusModule.generateNextBlocks(accounts)
    if (blocks.nonEmpty) {
      val bestBlock = blocks.max(consensusModule.blockOrdering)
      application.coordinator ! AddBlock(bestBlock, None)
      true
    } else false
  }.recoverWith {
    case ex =>
      log.error(s"Failed to generate new block: ${ex.getMessage}")
      Failure(ex)
  }.getOrElse(false)

  private def scheduleBlockGeneration(): Unit = {
    val schedule = if (application.settings.tflikeScheduling) {
      val lastBlock = application.history.lastBlock
      val currentTime = NTP.correctedTime()

      accounts
        .flatMap(acc => consensusModule.nextBlockGenerationTime(lastBlock, acc).map(_ + BlockGenerationTimeShift.toMillis))
        .map(t => math.max(t - currentTime, blockGenerationDelay.toMillis))
        .filter(_ < MaxBlockGenerationDelay.toMillis)
        .map(_ millis)
        .distinct.sorted
    } else Seq.empty

    val tasks = if (schedule.isEmpty) {
      log.info(s"Next block generation will start in $blockGenerationDelay")
      setSchedule(Seq(blockGenerationDelay))
    } else {
      val firstN = 7
      log.info(s"Block generation schedule: ${schedule.take(firstN).mkString(", ")}...")
      setSchedule(schedule)
    }

    currentState = Some(tasks)
  }

  private def setSchedule(schedule: Seq[FiniteDuration]): Seq[Cancellable] = {
    val systemScheduler = context.system.scheduler
    schedule.map { t => systemScheduler.schedule(t, FailedGenerationDelay, self, GenerateBlock) }
  }
}

object Miner {

  case class GuessABlock(rescheduleImmediately: Boolean)

  case object Stop

  private case object GenerateBlock

  private[mining] val BlockGenerationTimeShift = 1 second

  private[mining] val FailedGenerationDelay = 10 seconds

  private[mining] val MaxBlockGenerationDelay = 1 hour
}
