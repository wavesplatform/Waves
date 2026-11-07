package com.wavesplatform.mining

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt, TestSchedulerOps}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.wallet.Wallet
import monix.execution.schedulers.TestScheduler
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.EitherValues
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

class NgMinerWithSponsorshipSuite extends FreeSpec with WithMiner with TestSchedulerOps with EitherValues {
  private val thisNodeAcc = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)

  private val baseSettings = DomainPresets.TransactionStateSnapshot
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .setFeaturesHeight(BlockchainFeatures.BlockRewardDistribution -> 100)

  private val featureCheckBlocksPeriod = baseSettings.blockchainSettings.functionalitySettings.featureCheckBlocksPeriod

  "forges a block on top of a microblock with a fee" - {
    "after the sponsored fees switch height" in forgesAfterFeeMicroBlock(feeSponsorshipHeight = -featureCheckBlocksPeriod)

    // Repro of https://github.com/wavesplatform/Waves/issues/4128: the miner's own append rejects block 3 with InvalidStateHash
    "between FeeSponsorship activation and the sponsored fees switch height" ignore forgesAfterFeeMicroBlock(feeSponsorshipHeight = 0)
  }

  private def forgesAfterFeeMicroBlock(feeSponsorshipHeight: Int): Unit = {
    val settings = baseSettings
      .setFeaturesHeight(BlockchainFeatures.FeeSponsorship -> feeSponsorshipHeight)
      .copy(minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = 100.millis))
      .configure(
        _.copy(
          daoAddress = None,
          xtnBuybackAddress = None,
          lightNodeBlockFieldsAbsenceInterval = 2,
          generationPeriodLength = 4
        )
      )

    val minerScheduler    = TestScheduler()
    val appenderScheduler = TestScheduler()
    val utxEvents         = ConcurrentSubject.publish[Unit](using minerScheduler)

    withDomainAndTestMiner(settings, Seq(AddrWithBalance(thisNodeAcc.toAddress, 10_000.waves)), minerScheduler, appenderScheduler, utxEvents) {
      (d, miner) =>
        // Key blocks are forged and appended on the appender; the miner scheduler only runs microblock mining
        def tickUntil(label: String)(done: => Boolean): Unit =
          Iterator.from(1).takeWhile(i => !done && i <= 20).foreach { i =>
            appenderScheduler.tickNext(s"$label-appender-$i", failIfNoTasks = false)
            if (!done) minerScheduler.tickNext(s"$label-miner-$i", failIfNoTasks = false)
          }

        log.debug("Schedule mining")
        miner.scheduleMining()

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
        d.blockchain.height shouldBe 3
    }
  }
}
