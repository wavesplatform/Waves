package com.wavesplatform.finalization

import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{CatchLogs, NumericExt, TestSchedulerOps, TestTime}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.schedulers.TestScheduler
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

class MultipleAccountsMinerWithFinalitySuite extends BaseFinalizationSpec, TestSchedulerOps {
  private val acc1 = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
  private val acc2 = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 1)

  private val otherNodeAcc1     = TxHelpers.defaultSigner
  private val otherNodeAcc2Addr = TxHelpers.secondAddress

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
    .copy(minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = 100.millis))
    .configure(_.copy(generationPeriodLength = 2))

  "Not committed Account 2 produces a valid block when committed Account 1 spends all waves" in withManager { manager =>
    val minerAcc                  = acc1
    val notCommittedAcc           = acc2
    val otherNodeNotCommittedAcc1 = otherNodeAcc1

    val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
    var miner    = Miner.StrictDisabledMiner
    val time     = TestTime()
    withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(minerAcc, notCommittedAcc, otherNodeNotCommittedAcc1),
      miner = Miner.forwardTo(miner),
      time = time
    ) { d =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      d.wallet.generateNewAccounts(2)

      val utxEvents = ConcurrentSubject.publish[Unit](using minerScheduler)
      val minerImpl = new MinerImpl(
        channels,
        d.blockchain,
        d.settings,
        time,
        d.utxPool,
        BlockEndorser.Disabled,
        EndorsementStorage.Disabled,
        d.wallet,
        d.posSelector,
        minerScheduler,
        appenderScheduler,
        utxEvents
      ) with CatchLogs
      miner = minerImpl

      log.debug("Append block2")
      val block2 = d.createBlock(
        txs = Seq(TxHelpers.commitToGeneration(Height(3), sender = minerAcc)),
        generator = otherNodeNotCommittedAcc1,
        strictTime = true
      )
      d.appender.appendBlock(block2)

      log.debug("Trigger forging block3")
      time.setTimeIfGreater(d.nextBlockTime(minerAcc))
      appenderScheduler.tickNext("appender-1")
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")
      withClue("Make sure, test works as expected: ") {
        d.lastBlock.header.generator.toAddress shouldBe minerAcc.toAddress
      }

      log.debug("Trigger microblock forging with spending all waves")
      d.utxPool.putIfNew(
        TxHelpers.transfer(
          minerAcc,
          otherNodeAcc2Addr,
          amount = d.blockchain.balance(minerAcc.toAddress) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
          fee = 1.waves,
          timestamp = time.getTimestamp()
        )
      )
      utxEvents.onNext(())
      minerScheduler.tickNext("miner-2")
      appenderScheduler.tickNext("appender-3")
      val liquidBlock1 = d.lastBlockId

      log.debug("Trigger next block forging")
      time.setTimeIfGreater(d.nextBlockTime(notCommittedAcc))
      minerScheduler.tickNext("miner-3")
      appenderScheduler.tickNext("appender-4")
      minerScheduler.tickNext("miner-4")
      appenderScheduler.tickNext("appender-5")
      withClue("Forged: ") {
        d.lastBlockId should not be liquidBlock1
        d.blockchain.height shouldBe 4
      }
      withClue(s"Other miner, minerAcc=${minerAcc.toAddress}: ") {
        d.lastBlock.header.generator.toAddress shouldBe notCommittedAcc.toAddress
      }
    }
  }

  "Not committed Account 2 produces an invalid block" - {
    "its attempt to forge doesn't stop current mining of Account 1" in withManager { manager =>
      val minerAcc               = acc1
      val notCommittedAcc        = acc2
      val otherNodeCommittedAcc1 = otherNodeAcc1

      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner    = Miner.StrictDisabledMiner
      val time     = TestTime()
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(minerAcc, notCommittedAcc, otherNodeCommittedAcc1),
        miner = Miner.forwardTo(miner),
        time = time
      ) { d =>
        val minerScheduler    = TestScheduler()
        val appenderScheduler = TestScheduler()

        d.wallet.generateNewAccounts(2)

        val utxEvents = ConcurrentSubject.publish[Unit](using minerScheduler)
        val minerImpl = new MinerImpl(
          channels,
          d.blockchain,
          d.settings,
          time,
          d.utxPool,
          BlockEndorser.Disabled,
          EndorsementStorage.Disabled,
          d.wallet,
          d.posSelector,
          minerScheduler,
          appenderScheduler,
          utxEvents
        ) with CatchLogs
        miner = minerImpl

        log.debug("Append block2")
        val block2 = d.createBlock(
          txs = Seq(minerAcc, otherNodeCommittedAcc1).map(x => TxHelpers.commitToGeneration(Height(3), sender = x)),
          generator = otherNodeCommittedAcc1,
          strictTime = true
        )
        d.appender.appendBlock(block2)

        log.debug("Trigger forging block3")
        time.setTimeIfGreater(d.nextBlockTime(minerAcc))
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")
        withClue("Make sure, test works as expected: ") {
          d.lastBlock.header.generator.toAddress shouldBe minerAcc.toAddress
        }

        log.debug("Trigger microblock forging with spending all waves")
        d.utxPool.putIfNew(
          TxHelpers.transfer(
            minerAcc,
            otherNodeAcc2Addr,
            amount = d.blockchain.balance(minerAcc.toAddress) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
            fee = 1.waves,
            timestamp = time.getTimestamp()
          )
        )
        utxEvents.onNext(())
        minerScheduler.tickNext("miner-2")
        appenderScheduler.tickNext("appender-3")
        val liquidBlock1 = d.lastBlockId

        log.debug("Trigger next block forging")
        time.setTimeIfGreater(d.nextBlockTime(notCommittedAcc))
        minerScheduler.tickNext("miner-3")
        appenderScheduler.tickNext("appender-4")
        minerScheduler.tickNext("miner-4") // Second time because of two accounts on node
        appenderScheduler.tickNext("appender-5")
        withClue("Not forged: ") {
          d.lastBlockId shouldBe liquidBlock1
        }

        log.debug("Trigger next micro block forging")
        d.utxPool.putIfNew(
          TxHelpers.transfer(
            otherNodeCommittedAcc1,
            otherNodeAcc2Addr,
            timestamp = time.getTimestamp()
          )
        )
        utxEvents.onNext(())
        minerScheduler.tickNext("miner-5")
        minerScheduler.tickNext("miner-6")
        appenderScheduler.tickNext("appender-6")
        withClue("Something forged: ") {
          d.lastBlockId should not be liquidBlock1
        }

        withClue(s"Still same miner, notCommittedAcc=${notCommittedAcc.toAddress}, otherNodeCommittedAcc1=${otherNodeCommittedAcc1.toAddress}: ") {
          d.lastBlock.header.generator.toAddress shouldBe minerAcc.toAddress
        }
      }
    }

    "it tries again after new microblock of Account 1" in withManager { manager =>
      val minerAcc               = acc1
      val notCommittedAcc        = acc2
      val otherNodeCommittedAcc1 = otherNodeAcc1

      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner    = Miner.StrictDisabledMiner
      val time     = TestTime()
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(minerAcc, notCommittedAcc, otherNodeCommittedAcc1),
        miner = Miner.forwardTo(miner),
        time = time
      ) { d =>
        val minerScheduler    = TestScheduler()
        val appenderScheduler = TestScheduler()

        d.wallet.generateNewAccounts(2)

        val utxEvents = ConcurrentSubject.publish[Unit](using minerScheduler)
        val minerImpl = new MinerImpl(
          channels,
          d.blockchain,
          d.settings,
          time,
          d.utxPool,
          BlockEndorser.Disabled,
          EndorsementStorage.Disabled,
          d.wallet,
          d.posSelector,
          minerScheduler,
          appenderScheduler,
          utxEvents
        ) with CatchLogs
        miner = minerImpl

        log.debug("Append block2")
        val block2 = d.createBlock(
          txs = Seq(minerAcc, otherNodeCommittedAcc1).map(x => TxHelpers.commitToGeneration(Height(3), sender = x)),
          generator = otherNodeCommittedAcc1,
          strictTime = true
        )
        d.appender.appendBlock(block2)

        log.debug("Trigger forging block3")
        time.setTimeIfGreater(d.nextBlockTime(minerAcc))
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")
        withClue("Make sure, test works as expected: ") {
          d.lastBlock.header.generator.toAddress shouldBe minerAcc.toAddress
        }

        log.debug("Trigger microblock forging with spending all waves by minerAcc")
        d.utxPool.putIfNew(
          TxHelpers.transfer(
            minerAcc,
            otherNodeAcc2Addr,
            amount = d.blockchain.balance(minerAcc.toAddress) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
            fee = 1.waves,
            timestamp = time.getTimestamp()
          )
        )
        utxEvents.onNext(())
        minerScheduler.tickNext("miner-2")
        appenderScheduler.tickNext("appender-3")
        val liquidBlock1 = d.lastBlockId

        log.debug("Trigger next block forging - first attempt")
        time.setTimeIfGreater(d.nextBlockTime(notCommittedAcc))
        minerScheduler.tickNext("miner-3")
        appenderScheduler.tickNext("appender-4")
        minerScheduler.tickNext("miner-4") // Second time because of two accounts on node
        appenderScheduler.tickNext("appender-5")
        withClue("Not forged: ") {
          d.lastBlockId shouldBe liquidBlock1
        }

        log.debug("Trigger next micro block forging")
        d.utxPool.putIfNew(
          TxHelpers.transfer(
            otherNodeCommittedAcc1,
            otherNodeAcc2Addr,
            amount = d.blockchain.balance(otherNodeCommittedAcc1.toAddress) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
            fee = 1.waves,
            timestamp = time.getTimestamp()
          )
        )
        utxEvents.onNext(())
        minerScheduler.tickNext("miner-5")
        minerScheduler.tickNext("miner-6")
        appenderScheduler.tickNext("appender-6")
        withClue("Something forged: ") {
          d.lastBlockId should not be liquidBlock1
        }

        withClue(s"Still same miner, notCommittedAcc=${notCommittedAcc.toAddress}, otherNodeCommittedAcc1=${otherNodeCommittedAcc1.toAddress}: ") {
          d.lastBlock.header.generator.toAddress shouldBe minerAcc.toAddress
        }

        log.debug("Trigger next block forging - second attempt")
        time.advance(defaultSettings.minerSettings.minMicroBlockAge)
        time.setTimeIfGreater(d.nextBlockTime(notCommittedAcc))
        minerScheduler.tickNext("miner-7")
        appenderScheduler.tickNext("appender-7")
        withClue("Not committed forged: ") {
          d.lastBlock.header.generator.toAddress shouldBe notCommittedAcc.toAddress
        }
      }
    }
  }
}
