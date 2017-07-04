package com.wavesplatform.mining

import java.time.{Duration, Instant}
import java.util.concurrent._

import com.google.common.util.concurrent.{FutureCallback, Futures, MoreExecutors}
import com.wavesplatform.settings.{BlockchainSettings, MinerSettings}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.PoSCalc._
import scorex.transaction.UnconfirmedTransactionsStorage.packUnconfirmed
import scorex.transaction.{History, UnconfirmedTransactionsStorage}
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.math.Ordering.Implicits._


class Miner(
    history: History,
    state: StateReader,
    utx: UnconfirmedTransactionsStorage,
    privateKeyAccounts: => Seq[PrivateKeyAccount],
    blockchainSettings: BlockchainSettings,
    minerSettings: MinerSettings,
    time: => Long,
    peerCount: => Int,
    blockHandler: Block => Unit) extends ScorexLogging {
  import Miner._

  private val minerPool = MoreExecutors.listeningDecorator(Executors.newScheduledThreadPool(2))
  private val scheduledAttempts = new ConcurrentHashMap[ByteStr, ScheduledFuture[_]]()

  private def checkAge(parentHeight: Int, parent: Block): Either[String, Unit] =
    Either
      .cond(parentHeight == 1, (), Duration.between(Instant.ofEpochMilli(parent.timestamp), Instant.ofEpochMilli(time)))
      .left.flatMap(blockAge => Either.cond(
      blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed,
      (),
      s"Blockchain is too old (last block ${parent.uniqueId} generated $blockAge ago)"
    ))

  private def generateBlock(account: PrivateKeyAccount, parentHeight: Int, parent: Block, greatGrandParent: Option[Block], balance: Long): Callable[Option[Block]] = () => {
    val pc = peerCount
    if (pc >= minerSettings.quorum) {
        val lastBlockKernelData = parent.consensusData
        val currentTime = time
        val h = calcHit(lastBlockKernelData, account)
        val t = calcTarget(parent, currentTime, balance)
        if (h < t) {
          log.debug(s"Forging with ${account.address}, H $h < T $t, balance $balance, prev block ${parent.uniqueId}")
          log.debug(s"Previous block ID ${parent.encodedId} at $parentHeight with target ${lastBlockKernelData.baseTarget}")

          val avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
          val btg = calcBaseTarget(avgBlockDelay, parentHeight, parent, greatGrandParent, currentTime)
          val gs = calcGeneratorSignature(lastBlockKernelData, account)
          val consensusData = NxtLikeConsensusBlockData(btg, gs)

          val unconfirmed = packUnconfirmed(state, blockchainSettings.functionalitySettings, utx, time, parentHeight)
          log.debug(s"Adding ${unconfirmed.size} unconfirmed transaction(s) to new block")

          Some(Block.buildAndSign(Version, currentTime, parent.uniqueId, consensusData, unconfirmed, account))
        } else {
          log.warn(s"Hit $h was NOT less than target $t, not forging block with ${account.address}")
          None
        }
    } else {
      log.debug(s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
      None
    }
  }

  private def retry(parentHeight: Int, parent: Block, greatGrandParent: Option[Block])(account: PrivateKeyAccount): Unit =
    // Next block generation time can be None only when the account's generating balance is less than required. The
    // balance, in turn, changes only when new block is appended to the blockchain, which causes all scheduled forging
    // attempts to be cancelled and re-scheduled over the new parent.
  {
    val calculatedTimestamp = for {
      _ <- checkAge(parentHeight, parent)
      ts <- nextBlockGenerationTime(parentHeight, state, blockchainSettings.functionalitySettings, parent, account)
    } yield ts

    calculatedTimestamp match {
      case Left(e) => log.debug(s"NOT scheduling block generation: $e")
      case Right(ts) =>
        val generationInstant = Instant.ofEpochMilli(ts + 10)
        val calculatedOffset = Duration.between(Instant.ofEpochMilli(time), generationInstant)
        val generationOffset = MinimalGenerationOffset.max(calculatedOffset)
        val balance = generatingBalance(state, blockchainSettings.functionalitySettings, account, parentHeight)

        scheduledAttempts.compute(ByteStr(account.publicKey), { (key, prevTask) =>
          if (prevTask != null) {
            log.debug(s"Block generation already scheduled for $key")
            prevTask
          } else if (!minerPool.isShutdown) {
            log.debug(s"Next attempt in $calculatedOffset ${if (calculatedOffset == generationOffset) "" else s"(adjusted to $generationOffset)"} " +
              s"at $generationInstant with ${account.address}, parent ${parent.uniqueId}")
            val thisAttempt = minerPool.schedule(generateBlock(account, parentHeight, parent, greatGrandParent, balance),
              generationOffset.toMillis, TimeUnit.MILLISECONDS)

            Futures.addCallback(thisAttempt, new FutureCallback[Option[Block]] {
              override def onSuccess(result: Option[Block]): Unit = result match {
                case Some(block) => blockHandler(block)
                case None =>
                  if (scheduledAttempts.remove(key, thisAttempt)) {
                    log.debug(s"No block generated: Retrying")
                    retry(parentHeight, parent, greatGrandParent)(account)
                  }
              }

              override def onFailure(t: Throwable): Unit = {}
            })

            thisAttempt
          } else null
        })
    }
  }

  def lastBlockChanged(parentHeight: Int, parent: Block): Unit = {
    log.debug(s"New parent block: ${parent.uniqueId},${if (scheduledAttempts.isEmpty) "" else s" cancelling ${scheduledAttempts.size()} task(s) and"} trying to schedule new attempts")
    scheduledAttempts.values.asScala.foreach(_.cancel(false))
    scheduledAttempts.clear()
    val greatGrandParent = history.parent(parent, 2)
    privateKeyAccounts.foreach(retry(parentHeight, parent, greatGrandParent))
  }

  def shutdown(): Unit = minerPool.shutdownNow()
}

object Miner extends ScorexLogging {
  private val minimalDurationMillis = 1001
  val Version: Byte = 2
  val MinimalGenerationOffset: Duration = Duration.ofMillis(minimalDurationMillis)
}
