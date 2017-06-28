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
import scorex.transaction.{History, PoSCalc, UnconfirmedTransactionsStorage}
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

  private def generateBlock(account: PrivateKeyAccount, parentHeight: Int, parent: Block, greatGrandParent: Option[Block]): Callable[Option[Block]] = () => {
    val blockAge = Duration.between(Instant.ofEpochMilli(parent.timestamp), Instant.ofEpochMilli(time))
    if (blockAge <= minerSettings.intervalAfterLastBlockThenGenerationIsAllowed) {
      val pc = peerCount
      if (pc >= minerSettings.quorum) {
        val balance = generatingBalance(state, blockchainSettings.functionalitySettings)(account, parentHeight)
        if (balance >= MinimalEffectiveBalanceForGenerator) {
          val lastBlockKernelData = parent.consensusData
          val currentTime = time
          val h = calcHit(lastBlockKernelData, account)
          val t = calcTarget(parent, currentTime, balance)
          if (h < t) {
            log.debug(s"Forging new block with ${account.address}, hit $h, target $t, balance $balance")
            log.debug(s"Previous block ID ${parent.uniqueId} at $parentHeight with target ${lastBlockKernelData.baseTarget}")

            val avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
            val btg = calcBaseTarget(avgBlockDelay, parentHeight, parent, greatGrandParent, currentTime)
            val gs = calcGeneratorSignature(lastBlockKernelData, account)
            val consensusData = NxtLikeConsensusBlockData(btg, gs)

            val unconfirmed = packUnconfirmed(state, blockchainSettings.functionalitySettings, utx, time, parentHeight)
            log.debug(s"Putting ${unconfirmed.size} unconfirmed transactions $blockAge after previous block")

            Some(Block.buildAndSign(Version, currentTime, parent.uniqueId, consensusData, unconfirmed, account))
          } else {
            log.warn(s"Hit $h was NOT less than target $t, not forging block with ${account.address}")
            None
          }
        } else {
          log.warn(s"Effective balance $balance is less that minimal ($MinimalEffectiveBalanceForGenerator) on ${account.address}")
          None
        }
      } else {
        log.warn(s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with ${account.address}")
        None
      }
    } else {
      log.warn(s"Blockchain is too old ($blockAge > ${minerSettings.intervalAfterLastBlockThenGenerationIsAllowed}), not forging block with ${account.address}")
      None
    }
  }

  private def retry(parentHeight: Int, parent: Block, greatGrandParent: Option[Block])(account: PrivateKeyAccount): Unit =
    // Next block generation time can be None only when the account's generating balance is less than required. The
    // balance, in turn, changes only when new block is appended to the blockchain, which causes all scheduled forging
    // attempts to be cancelled and re-scheduled over the new parent.
    for (ts <- PoSCalc.nextBlockGenerationTime(parentHeight, state, blockchainSettings.functionalitySettings, parent, account)) {
      val generationInstant = Instant.ofEpochMilli(ts + 10)
      val calculatedOffset = Duration.between(Instant.ofEpochMilli(time), generationInstant)
      log.debug(s"Next block generation attempt calculated in $calculatedOffset (parent: ${parent.uniqueId})")
      val generationOffset = MinimalGenerationOffset.max(calculatedOffset)
      log.debug(s"Next attempt in $generationOffset (${account.address} at $generationInstant)")

      scheduledAttempts.compute(ByteStr(account.publicKey), { (key, prevTask) =>
        if (prevTask != null) {
          log.debug(s"Block generation already scheduled for $key")
          prevTask
        } else {
          val thisAttempt = minerPool.schedule(
            generateBlock(account, parentHeight, parent, greatGrandParent),
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
        }
      })
    }

  def lastBlockChanged(parentHeight: Int, parent: Block): Unit = {
    log.debug(s"Canceling: Parent block: ${parent.uniqueId.toString}")
    scheduledAttempts.values.asScala.foreach(_.cancel(false))
    scheduledAttempts.clear()
    val greatGrandParent = history.parent(parent, 2)
    log.debug("Attempting to schedule block generation")
    privateKeyAccounts.foreach(retry(parentHeight, parent, greatGrandParent))
  }

  def shutdown(): Unit = minerPool.shutdownNow()
}

object Miner extends ScorexLogging {
  private val minimalDurationMillis = 1001
  val Version: Byte = 2
  val MinimalGenerationOffset: Duration = Duration.ofMillis(minimalDurationMillis)
}
