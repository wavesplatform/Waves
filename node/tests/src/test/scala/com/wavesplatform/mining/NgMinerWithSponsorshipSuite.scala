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
      log.debug("Schedule mining")
      minerImpl.scheduleMining()

      log.debug("Trigger forging block 2")
      d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
      appenderScheduler.tickNext("appender-1")
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")

      log.debug("Trigger forging microblock of 2")
      d.utxPool.putIfNew(TxHelpers.transfer(from = thisNodeAcc, amount = 10.waves, fee = 1.waves))
      utxEvents.onNext(())
      minerScheduler.tickNext("miner-2")
      appenderScheduler.tickNext("appender-3")
      minerScheduler.tickNext("miner-1")

      val lastBlockId = d.lastBlockId

      log.debug("Trigger forging block 3")
      d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
      appenderScheduler.tickNext("appender-1")
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")

      // TODO: fails, because the miner has an unexpected WAVES balance
      d.lastBlockId shouldNot be(lastBlockId)
    }
  }
}
