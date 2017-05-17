package scorex.consensus.mining

import akka.actor.{Actor, ActorRef, Cancellable}
import com.wavesplatform.settings.{BlockchainSettings, MinerSettings}
import com.wavesplatform.state2.reader.StateReader
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.network.Coordinator.AddBlock
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}
import scorex.wallet.Wallet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Try}

class Miner(minerSettings: MinerSettings,
            wallet: Wallet,
            history: History,
            stateReader: StateReader,
            blockchainSettings: BlockchainSettings,
            utxStorage: UnconfirmedTransactionsStorage,
            time: Time,
            coordinator: ActorRef) extends Actor with ScorexLogging {

  import scorex.consensus.mining.Miner._

  private lazy val blockGenerationDelay = math.max(minerSettings.generationDelay.toMillis, BlockGenerationTimeShift.toMillis) millis

  private var currentState = Option.empty[Seq[Cancellable]]

  private def accounts = wallet.privateKeyAccounts()

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


  private def tryToGenerateABlock(): Boolean = Try {
    log.debug("Trying to generate a new block")

    val blocks = generateNextBlocks(history, stateReader, blockchainSettings, utxStorage, time)(accounts)
    if (blocks.nonEmpty) {
      val bestBlock = blocks.max(PoSCalc.blockOrdering(history, stateReader, blockchainSettings.functionalitySettings, time))
      coordinator ! AddBlock(bestBlock, None)
      true
    } else false
  } recoverWith { case e =>
    log.warn(s"Failed to generate new block: ${e.getMessage}")
    Failure(e)
  } getOrElse false

  protected def preciseTime: Long = time.correctedTime()

  private def scheduleBlockGeneration(): Unit = try {
    val schedule = if (minerSettings.tfLikeScheduling) {
      val lastBlock = history.lastBlock
      val currentTime = preciseTime

      accounts
        .flatMap(acc => PoSCalc.nextBlockGenerationTime(history, stateReader, blockchainSettings.functionalitySettings, time)(lastBlock, acc).map(_ + BlockGenerationTimeShift.toMillis))
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
    case _: UnsupportedOperationException =>
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

object Miner extends ScorexLogging {

  case class GuessABlock(rescheduleImmediately: Boolean)

  case object Stop

  private case object GenerateBlock

  private[mining] val BlockGenerationTimeShift = 1 second

  private[mining] val MaxBlockGenerationDelay = 1 hour

  val Version: Byte = 2

  def generateNextBlocks(history: History, state: StateReader, bcs: BlockchainSettings, utx: UnconfirmedTransactionsStorage, time: Time)
                        (accounts: Seq[PrivateKeyAccount]): Seq[Block] = {

    import scorex.transaction.PoSCalc._

    def generateNextBlock(account: PrivateKeyAccount): Option[Block] = try {

      val lastBlock = history.lastBlock
      val height = history.heightOf(lastBlock).get
      val balance = generatingBalance(state, bcs.functionalitySettings)(account, height)

      if (balance < MinimalEffectiveBalanceForGenerator) {
        throw new IllegalStateException(s"Effective balance $balance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
      }

      val lastBlockKernelData = lastBlock.consensusData
      val currentTime = time.correctedTime()

      val h = calcHit(lastBlockKernelData, account)
      val t = calcTarget(lastBlock, currentTime, balance)

      val eta = (currentTime - lastBlock.timestamp) / 1000

      log.debug(s"hit: $h, target: $t, generating ${
        h < t
      }, eta $eta, " +
        s"account:  $account " +
        s"account balance: $balance " +
        s"last block id: ${
          lastBlock.encodedId
        }, " +
        s"height: $height, " +
        s"last block target: ${
          lastBlockKernelData.baseTarget
        }"
      )

      if (h < t) {

        val avgBlockDelay = bcs.genesisSettings.averageBlockDelay
        val btg = calcBaseTarget(history)(avgBlockDelay, lastBlock, currentTime)
        val gs = calcGeneratorSignature(lastBlockKernelData, account)
        val consensusData = NxtLikeConsensusBlockData(btg, gs)

        val unconfirmed = UnconfirmedTransactionsStorage.packUnconfirmed(state, bcs.functionalitySettings, utx, time, height)
        log.debug(s"Build block with ${
          unconfirmed.size
        } transactions")
        log.debug(s"Block time interval is $eta seconds ")

        Some(Block.buildAndSign(Version,
          currentTime,
          lastBlock.uniqueId,
          consensusData,
          unconfirmed,
          account))
      } else None
    } catch {
      case e: UnsupportedOperationException =>
        log.debug(s"DB can't find last block because of unexpected modification")
        None
      case e: IllegalStateException =>
        log.debug(s"Failed to generate new block: ${
          e.getMessage
        }")
        None
    }

    accounts.flatMap(generateNextBlock)
  }
}
