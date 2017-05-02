package scorex.consensus.mining

import akka.actor.{Actor, Cancellable}
import scorex.app.Application
import scorex.consensus.mining.Miner._
import scorex.network.Coordinator.AddBlock
import scorex.transaction.TransactionModule
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Try}

class Miner(application: Application) extends Actor with ScorexLogging {

  private lazy val blockGenerationDelay =
    math.max(application.settings.minerSettings.generationDelay.toMillis, BlockGenerationTimeShift.toMillis) millis


  private var currentState = Option.empty[Seq[Cancellable]]

  private def accounts = application.wallet.privateKeyAccounts()

  override def receive: Receive = {
    case GuessABlock(rescheduleImmediately) =>
      if (rescheduleImmediately) {
        cancel()
      }
      if (currentState.isEmpty) {
        scheduleBlockGeneration()
      }

    case GenerateBlock =>
      cancel()

      val blockGenerated = tryToGenerateABlock()

      if (!blockGenerated) {
        scheduleBlockGeneration()
      }

    case Stop => context stop self
  }

  override def postStop(): Unit = {
    cancel()
  }

  private val history = application.blockStorage.history

  private val state = application.blockStorage.stateReader

  private val bcs = application.settings.blockchainSettings

  private val utx = application.utxStorage

  private def tryToGenerateABlock(): Boolean = Try {
    log.debug("Trying to generate a new block")

    val blocks = TransactionModule.generateNextBlocks(history, state, bcs, utx, application.time)(accounts)
    if (blocks.nonEmpty) {
      val bestBlock = blocks.max(TransactionModule.blockOrdering(history, state, bcs.functionalitySettings, application.time))
      application.coordinator ! AddBlock(bestBlock, None)
      true
    } else false
  } recoverWith { case e =>
    log.warn(s"Failed to generate new block: ${e.getMessage}")
    Failure(e)
  } getOrElse false

  protected def preciseTime: Long = application.time.correctedTime()

  private def scheduleBlockGeneration(): Unit = try {
    val schedule = if (application.settings.minerSettings.tfLikeScheduling) {
      val lastBlock = history.lastBlock
      val currentTime = preciseTime

      accounts
        .flatMap(acc => TransactionModule.nextBlockGenerationTime(history, state, bcs.functionalitySettings, application.time)(lastBlock, acc).map(_ + BlockGenerationTimeShift.toMillis))
        .map(t => math.max(t - currentTime, blockGenerationDelay.toMillis))
        .filter(_ < MaxBlockGenerationDelay.toMillis)
        .map(_ millis)
        .distinct.sorted
    } else Seq.empty

    val tasks = if (schedule.isEmpty) {
      log.debug(s"Next block generation will start in $blockGenerationDelay")
      setSchedule(Seq(blockGenerationDelay))
    } else {
      val firstN = 3
      log.info(s"Block generation schedule: ${schedule.take(firstN).mkString(", ")}...")
      setSchedule(schedule)
    }

    currentState = Some(tasks)
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
  }

  private def cancel(): Unit = {
    currentState.toSeq.flatten.foreach(_.cancel())
    currentState = None
  }

  private def setSchedule(schedule: Seq[FiniteDuration]): Seq[Cancellable] = {
    val repeatIfNotDeliveredInterval = 10.seconds
    val systemScheduler = context.system.scheduler

    schedule.map { t => systemScheduler.schedule(t, repeatIfNotDeliveredInterval, self, GenerateBlock) }
  }
}

object Miner {

  case class GuessABlock(rescheduleImmediately: Boolean)

  case object Stop

  private case object GenerateBlock

  private[mining] val BlockGenerationTimeShift = 1 second

  private[mining] val MaxBlockGenerationDelay = 1 hour
}
