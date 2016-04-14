package scorex.consensus.mining

import akka.actor.Actor
import scorex.app.Application
import scorex.block.Block
import scorex.consensus.mining.Miner._
import scorex.utils.ScorexLogging

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class Miner(application: Application) extends Actor with ScorexLogging {

  // BlockGenerator is trying to generate a new block every $blockGenerationDelay. Should be 0 for PoW consensus model.
  val blockGenerationDelay = application.settings.blockGenerationDelay
  val BlockGenerationTimeLimit = 5.seconds

  var lastTryTime = 0L
  var stopped = false

  private def scheduleAGuess(delay: Option[FiniteDuration] = None): Unit =
    context.system.scheduler.scheduleOnce(delay.getOrElse(blockGenerationDelay), self, GuessABlock)

  scheduleAGuess(Some(0.millis))

  override def receive: Receive = {
    case GuessABlock =>
      stopped = false
      if (System.currentTimeMillis() - lastTryTime >= blockGenerationDelay.toMillis) tryToGenerateABlock()

    case GetLastGenerationTime =>
      sender ! LastGenerationTime(lastTryTime)

    case Stop =>
      stopped = true
  }

  def tryToGenerateABlock(): Unit = Try {
    implicit val transactionalModule = application.transactionModule

    lastTryTime = System.currentTimeMillis()
    if (blockGenerationDelay > 500.milliseconds) log.info("Trying to generate a new block")
    val accounts = application.wallet.privateKeyAccounts()
    val blocksFuture = application.consensusModule.generateNextBlocks(accounts)(application.transactionModule)
    val blocks: Seq[Block] = Await.result(blocksFuture, BlockGenerationTimeLimit)
    if (blocks.nonEmpty) application.historySynchronizer ! blocks.maxBy(application.consensusModule.blockScore)
    if (!stopped) scheduleAGuess()
  }.recoverWith {
    case ex =>
      log.error(s"Failed to generate new block: ${ex.getMessage}")
      Failure(ex)
  }

}

object Miner {

  case object Stop

  case object GuessABlock

  case object GetLastGenerationTime

  case class LastGenerationTime(time: Long)

}
