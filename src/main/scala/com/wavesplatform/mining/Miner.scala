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

  private def generateBlock(account: PrivateKeyAccount, parentHeight: Int, parent: Block, greatGrandParent: Option[Block]): Callable[Block] = () => {
    val publicKey = ByteStr(account.publicKey)
    val pc = peerCount
    val blockAge = Duration.between(Instant.ofEpochMilli(parent.timestamp), Instant.ofEpochMilli(time))
    require(pc >= minerSettings.quorum,
      s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with $publicKey")

    val balance = generatingBalance(state, blockchainSettings.functionalitySettings)(account, parentHeight)

    require(balance >= MinimalEffectiveBalanceForGenerator,
      s"Effective balance $balance is less that minimal ($MinimalEffectiveBalanceForGenerator)")

    val lastBlockKernelData = parent.consensusData
    val currentTime = time

    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(parent, currentTime, balance)

    require(h < t, s"Hit $h was NOT less than target $t")

    log.debug(s"hit=$h, target=$t, ${if (h < t) "" else "NOT"} generating, account=$account, " +
      s"balance=$balance, lastBlockId=${parent.encodedId}, height=$parentHeight, lastTarget=${lastBlockKernelData.baseTarget}")

    val avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
    val btg = calcBaseTarget(avgBlockDelay, parentHeight, parent, greatGrandParent, currentTime)
    val gs = calcGeneratorSignature(lastBlockKernelData, account)
    val consensusData = NxtLikeConsensusBlockData(btg, gs)

    val unconfirmed = packUnconfirmed(state, blockchainSettings.functionalitySettings, utx, time, parentHeight)
    log.info(s"\n\n${unconfirmed.mkString("\n")}\n\n")
    log.debug(s"Generating block with ${unconfirmed.size} transactions $blockAge after previous block")

    Block.buildAndSign(Version, currentTime, parent.uniqueId, consensusData, unconfirmed, account)
  }

  private def retry(parentHeight: Int, parent: Block, greatGrandParent: Option[Block])(account: PrivateKeyAccount): Unit =
    // Next block generation time can be None only when the account's generating balance is less than required. The
    // balance, in turn, changes only when new block is appended to the blockchain, which causes all scheduled forging
    // attempts to be cancelled and re-scheduled over the new parent.
    for (ts <- PoSCalc.nextBlockGenerationTime(parentHeight, state, blockchainSettings.functionalitySettings, parent, account)) {
      val generationInstant = Instant.ofEpochMilli(ts)

      val generationOffset = MinimalGenerationOffset.max(Duration.between(Instant.ofEpochMilli(time), generationInstant))
      log.debug(s"Next attempt in $generationOffset (${account.address} at $generationInstant)")

      scheduledAttempts.compute(ByteStr(account.publicKey), { (key, prevTask) =>
        if (prevTask != null) {
          log.debug(s"Block generation already scheduled for $key")
          prevTask
        } else {
          val thisAttempt = minerPool.schedule(
            generateBlock(account, parentHeight, parent, greatGrandParent),
            generationOffset.toMillis, TimeUnit.MILLISECONDS)

          Futures.addCallback(thisAttempt, new FutureCallback[Block] {
            override def onSuccess(result: Block) = blockHandler(result)

            override def onFailure(t: Throwable) = t match {
              case _: CancellationException =>
                log.trace(s"Block generation cancelled for $key")
              case iae: IllegalArgumentException =>
                log.debug(s"Error generating block, retrying", iae)
                scheduledAttempts.remove(key, thisAttempt)
                retry(parentHeight, parent, greatGrandParent)(account)
            }
          })

          thisAttempt
        }
      })
    }

  def lastBlockChanged(parentHeight: Int, parent: Block): Unit = {
    scheduledAttempts.values.asScala.foreach(_.cancel(true))
    scheduledAttempts.clear()
    val greatGrandParent = history.blockAt(parentHeight - 3)

    log.debug("Attempting to schedule block generation")
    privateKeyAccounts.foreach(retry(parentHeight, parent, greatGrandParent))
  }

  def shutdown(): Unit = minerPool.shutdownNow()
}

object Miner extends ScorexLogging {
  val Version: Byte = 2
  val MinimalGenerationOffset = Duration.ofMillis(1001)
}
