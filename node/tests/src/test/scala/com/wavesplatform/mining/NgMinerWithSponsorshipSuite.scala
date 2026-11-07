package com.wavesplatform.mining

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt, TestSchedulerOps, WithResourceManager}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.schedulers.TestScheduler
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.EitherValues
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

class NgMinerWithSponsorshipSuite extends FreeSpec with WithDomain with TestSchedulerOps with WithResourceManager with EitherValues {
  private val thisNodeAcc = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)

  // Known bug, kept as a repro: the miner forges block 3 on top of a sponsored-fee microblock and its own append rejects it with InvalidStateHash
  "Correct block signature" ignore withManager { manager =>
    val minerScheduler    = TestScheduler()
    val appenderScheduler = TestScheduler()

    val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
    var miner    = Miner.StrictDisabledMiner

    val baseSettings = DomainPresets.TransactionStateSnapshot
      .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
      .setFeaturesHeight(
        BlockchainFeatures.FeeSponsorship          -> 0, // The root of issue, it works with -feature_period
        BlockchainFeatures.BlockRewardDistribution -> 100
      )

    val settings = baseSettings
      .copy(minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = 100.millis))
      .configure(
        _.copy(
          daoAddress = None,
          xtnBuybackAddress = None,
          lightNodeBlockFieldsAbsenceInterval = 2,
          generationPeriodLength = 4
        )
      )

    withDomain(settings, Seq(AddrWithBalance(thisNodeAcc.toAddress, 10_000.waves)), miner = Miner.forwardTo(miner)) { d =>
      d.wallet.generateNewAccounts(1)

      val endorsementStorage = EndorsementStorage.InMemory((blockId, h) => blockId == d.blockchain.blockId(h.toInt))
      val blockEndorser = BlockEndorser.InMemory(d.settings.synchronizationSettings.maxRollback, d.blockchain, d.wallet, endorsementStorage, channels)
      val utxEvents     = ConcurrentSubject.publish[Unit](using minerScheduler)
      val minerImpl = new MinerImpl(
        channels,
        d.blockchain,
        d.settings,
        d.testTime,
        d.utxPool,
        blockEndorser,
        endorsementStorage,
        d.wallet,
        d.posSelector,
        minerScheduler,
        appenderScheduler,
        utxEvents
      )
      miner = minerImpl
      // The genesis timestamp comes from the JVM-wide TxHelpers counter, which can lag TestTime by minutes, so an attempt would fire at once
      d.testTime.setTime(d.lastBlock.header.timestamp)

      // Key blocks are forged and appended on the appender; the miner scheduler only runs microblock mining
      def tickUntil(label: String)(done: => Boolean): Unit =
        Iterator.from(1).takeWhile(i => !done && i <= 20).foreach { i =>
          appenderScheduler.tickNext(s"$label-appender-$i", failIfNoTasks = false)
          if (!done) minerScheduler.tickNext(s"$label-miner-$i", failIfNoTasks = false)
        }

      log.debug("Schedule mining")
      minerImpl.scheduleMining()

      log.debug("Trigger forging block 2")
      d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
      tickUntil("block-2")(d.blockchain.height == 2)
      d.blockchain.height shouldBe 2

      log.debug("Trigger forging microblock of 2")
      val block2Id = d.lastBlockId
      d.utxPool.putIfNew(TxHelpers.transfer(from = thisNodeAcc, amount = 10.waves, fee = 1.waves))
      utxEvents.onNext(())
      tickUntil("micro-2")(d.lastBlockId != block2Id)
      d.lastBlockId shouldNot be(block2Id)

      log.debug("Trigger forging block 3")
      d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
      tickUntil("block-3")(d.blockchain.height == 3)

      // TODO: fails, MinerImpl logs "Error mining block ...: InvalidStateHash(...)"
      d.blockchain.height shouldBe 3
    }
  }
}
