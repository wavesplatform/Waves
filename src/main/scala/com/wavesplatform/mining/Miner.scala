package com.wavesplatform.mining

import java.time.{Duration, Instant}
import java.util.concurrent.{Executors, ScheduledFuture, TimeUnit}

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

import scala.collection.mutable
import scala.math.Ordering.Implicits._
import scala.util.control.NonFatal

/* This class is NOT SYNCHRONIZED AT ALL, it is meant to be used from Coordinator only! */
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

  private val minerPool = Executors.newScheduledThreadPool(2)
  private var scheduledFutures = mutable.AnyRefMap.empty[ByteStr, ScheduledFuture[_]]

  def lastBlockChanged(parentHeight: Int, parent: Block): Unit = {
    val greatGrandParent = history.blockAt(parentHeight - 3)
    for (account <- privateKeyAccounts; ts <- PoSCalc.nextBlockGenerationTime(parentHeight, state, blockchainSettings.functionalitySettings, parent, account)) {
      val generationInstant = Instant.ofEpochMilli(ts)
      val generationOffset = Math.max(MinimalGenerationOffset, ts - time)
      log.debug(s"Next attempt in ${Duration.ofMillis(generationOffset)} (${account.address} at $generationInstant)")

      val publicKey = ByteStr(account.publicKey)
      scheduledFutures.get(publicKey).foreach(_.cancel(false))
      scheduledFutures += publicKey -> minerPool.schedule((() => {
        val pc = peerCount
        val blockAge = Duration.between(Instant.ofEpochMilli(parent.timestamp), Instant.ofEpochMilli(time))
        if (pc < minerSettings.quorum) {
          log.debug(s"Quorum not available ($pc/${minerSettings.quorum}, not forging block with $publicKey")
        } else if (blockAge > minerSettings.intervalAfterLastBlockThenGenerationIsAllowed) {
          log.debug(s"Parent block is too old (generated $blockAge ago)")
        } else try {
          val balance = generatingBalance(state, blockchainSettings.functionalitySettings)(account, parentHeight)

          require(balance >= MinimalEffectiveBalanceForGenerator,
            s"Effective balance $balance is less that minimal ($MinimalEffectiveBalanceForGenerator)")

          val lastBlockKernelData = parent.consensusData
          val currentTime = time

          val h = calcHit(lastBlockKernelData, account)
          val t = calcTarget(parent, currentTime, balance)

          require(h < t, s"Hit $h was NOT less than target $t")

          log.debug(s"hit=$h, target=$t, ${if (h < t) "" else "NOT"} generating, account=$account, " +
            s"balance=$balance, lastBlockId=${parent.uniqueId}, height=$parentHeight, lastTarget=${lastBlockKernelData.baseTarget}")

          val avgBlockDelay = blockchainSettings.genesisSettings.averageBlockDelay
          val btg = calcBaseTarget(avgBlockDelay, parentHeight, parent, greatGrandParent, currentTime)
          val gs = calcGeneratorSignature(lastBlockKernelData, account)
          val consensusData = NxtLikeConsensusBlockData(btg, gs)

          val unconfirmed = packUnconfirmed(state, blockchainSettings.functionalitySettings, utx, time, parentHeight)
          log.debug(s"Building block with ${unconfirmed.size} transactions $blockAge after previous block")

          blockHandler(Block.buildAndSign(Version, currentTime, parent.uniqueId, consensusData, unconfirmed, account))
        } catch {
          case NonFatal(e) => log.warn("Error generating block", e)
        }
      }): Runnable, generationOffset, TimeUnit.MILLISECONDS)

    }
  }

  def shutdown(): Unit = minerPool.shutdownNow()
}

object Miner {
  val Version: Byte = 2
  val MinimalGenerationOffset = 1001
}
