package com.wavesplatform.finalization

import com.wavesplatform.TestValues
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.consensus.GeneratingBalanceProvider.MinimalEffectiveBalanceForGenerator2
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.network.EndorseBlock
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{CatchLogs, NumericExt, TestSchedulerOps, TestTime}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

class MinerWithFinalitySuite extends BaseFinalizationSpec, TestSchedulerOps {
  private val thisNodeAcc  = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
  private val otherNodeAcc = TxHelpers.defaultSigner

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
    .copy(minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = 100.millis))
    .configure(_.copy(generationPeriodLength = 2))

  "Mining works on new period even" - {
    "committed after scheduled time" - pending

    "committed in the last block of period" in withManager { manager =>
      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner    = Miner.StrictDisabledMiner
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc) ++ Seq(
          AddrWithBalance(
            thisNodeAcc.toAddress,
            MinimalEffectiveBalanceForGenerator2 + TestValues.commitToGenerationFee + CommitToGenerationTransaction.DepositInWavelets
          )
        ),
        miner = Miner.forwardTo(miner)
      ) { d =>
        val minerScheduler    = TestScheduler()
        val appenderScheduler = TestScheduler()

        d.wallet.generateNewAccounts(1)

        val minerImpl = new MinerImpl(
          channels,
          d.blockchain,
          d.settings,
          d.testTime,
          d.utxPool,
          BlockEndorser.Disabled,
          EndorsementStorage.Disabled,
          d.wallet,
          d.posSelector,
          minerScheduler,
          appenderScheduler,
          Observable.empty
        ) with CatchLogs
        miner = minerImpl

        log.debug("Append block2")
        val block2 = d.createBlock(generator = otherNodeAcc, strictTime = true)
        d.appender.appendBlock(block2)
        d.appendMicroBlock(TxHelpers.commitToGeneration(Height(3), sender = thisNodeAcc))
        d.utxPool.cleanUnconfirmed()

        log.debug("Trigger thisNode forging")
        d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.lastBlock.header.generator.toAddress shouldBe thisNodeAcc.toAddress
        minerImpl.inMemoryLog.getMessages.find(_.contains("is not committed on 3")) shouldBe empty
      }
    }

    "was conflict in previous period" in withManager { manager =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner    = Miner.StrictDisabledMiner
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc) ++ Seq(
          AddrWithBalance(
            thisNodeAcc.toAddress,
            MinimalEffectiveBalanceForGenerator2 + TestValues.commitToGenerationFee + CommitToGenerationTransaction.DepositInWavelets
          )
        ),
        miner = Miner.forwardTo(miner)
      ) { d =>
        d.wallet.generateNewAccounts(1)

        val minerImpl = new MinerImpl(
          channels,
          d.blockchain,
          d.settings,
          d.testTime,
          d.utxPool,
          BlockEndorser.Disabled,
          EndorsementStorage.Disabled,
          d.wallet,
          d.posSelector,
          minerScheduler,
          appenderScheduler,
          Observable.empty
        ) with CatchLogs
        miner = minerImpl

        log.debug("Append block2 with commitments")
        val txs                   = Seq(otherNodeAcc, thisNodeAcc).map(x => TxHelpers.commitToGeneration(Height(3), sender = x))
        val block2WithCommitments = d.createBlock(txs, generator = otherNodeAcc, strictTime = true)
        d.appender.appendBlock(block2WithCommitments)

        log.debug("Append block3 with conflict")
        val block3WithVotes = d.createBlock(
          generator = otherNodeAcc,
          strictTime = true,
          finalizationVoting = Some(mkFinalizationVoting().withConflict(thisNodeAcc, GeneratorIndex(1), block2WithCommitments.id()))
        )
        d.appender.appendBlock(block3WithVotes)

        log.debug("Append empty block")
        d.appender.appendBlock(d.createBlock(generator = otherNodeAcc, strictTime = true))
        val block5Id = d.lastBlockId

        log.debug("Trigger thisNode forging")
        d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.lastBlockId should not be block5Id
      }
    }
  }

  "Mining works if not committed, but all generators have no right to mine" - {
    def test(makeGeneratorSetEmptyF: Domain => Unit): Unit = withManager { manager =>
      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner    = Miner.StrictDisabledMiner
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc, thisNodeAcc), // Only otherNodeAcc committed
        miner = Miner.forwardTo(miner)
      ) { d =>
        val minerScheduler    = TestScheduler()
        val appenderScheduler = TestScheduler()

        d.wallet.generateNewAccounts(1)

        val minerImpl = new MinerImpl(
          channels,
          d.blockchain,
          d.settings,
          d.testTime,
          d.utxPool,
          BlockEndorser.Disabled,
          EndorsementStorage.Disabled,
          d.wallet,
          d.posSelector,
          minerScheduler,
          appenderScheduler,
          Observable.empty
        ) with CatchLogs
        miner = minerImpl

        log.debug("Append block2")
        val block2 = d.createBlock(
          txs = Seq(TxHelpers.commitToGeneration(Height(3), sender = otherNodeAcc)),
          generator = otherNodeAcc,
          strictTime = true
        )
        d.appender.appendBlock(block2)

        log.debug("Spending")
        makeGeneratorSetEmptyF(d)

        log.debug("Trigger thisNode forging")
        d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.lastBlock.header.generator.toAddress shouldBe thisNodeAcc.toAddress
      }
    }

    // There are no tests with conflicting generators, because the miner can't be conflicting, it can only spend all WAVES

    "spending in" - {
      "block" in test { d =>
        log.debug("Append block3 with spending all waves by miner")
        val block3 = d.createBlock(
          txs = Seq(
            TxHelpers.transfer(
              otherNodeAcc,
              thisNodeAcc.toAddress,
              amount = d.blockchain.balance(thisNodeAcc.toAddress) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
              fee = 1.waves
            )
          ),
          generator = otherNodeAcc,
          strictTime = true
        )
        d.appender.appendBlock(block3)
      }

      "microblock" in test { d =>
        log.debug("Append micro block with spending all waves by miner")
        d.appender.appendBlock(d.createBlock(generator = otherNodeAcc, strictTime = true))
        d.appendMicroBlock(
          d.createMicroBlock(signer = Some(otherNodeAcc))(
            TxHelpers.transfer(
              otherNodeAcc,
              thisNodeAcc.toAddress,
              amount = d.blockchain.balance(thisNodeAcc.toAddress) - CommitToGenerationTransaction.DepositInWavelets - 1.waves,
              fee = 1.waves
            )
          )
        )
      }
    }
  }

  "Mining doesn't work" - {
    "if conflict" in withManager { manager =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner    = Miner.StrictDisabledMiner
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc) ++ Seq(
          AddrWithBalance(
            thisNodeAcc.toAddress,
            MinimalEffectiveBalanceForGenerator2 + TestValues.commitToGenerationFee + CommitToGenerationTransaction.DepositInWavelets
          )
        ),
        miner = Miner.forwardTo(miner)
      ) { d =>
        d.wallet.generateNewAccounts(1)

        val minerImpl = new MinerImpl(
          channels,
          d.blockchain,
          d.settings,
          d.testTime,
          d.utxPool,
          BlockEndorser.Disabled,
          EndorsementStorage.Disabled,
          d.wallet,
          d.posSelector,
          minerScheduler,
          appenderScheduler,
          Observable.empty
        ) with CatchLogs
        miner = minerImpl

        log.debug("Append block2 with commitments")
        val txs                   = Seq(otherNodeAcc, thisNodeAcc).map(x => TxHelpers.commitToGeneration(Height(3), sender = x))
        val block2WithCommitments = d.createBlock(txs, generator = otherNodeAcc, strictTime = true)
        d.appender.appendBlock(block2WithCommitments)

        log.debug("Append block3 with conflict")
        val block3WithVotes = d.createBlock(
          generator = otherNodeAcc,
          strictTime = true,
          finalizationVoting = Some(mkFinalizationVoting().withConflict(thisNodeAcc, GeneratorIndex(1), block2WithCommitments.id()))
        )
        d.appender.appendBlock(block3WithVotes)

        log.debug("Trigger thisNode forging")
        d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.lastBlockId shouldBe block3WithVotes.id() // Not changed
        minerImpl.inMemoryLog.getMessages.find(_.contains("is conflict on 4")) should not be empty
      }
    }

    "if not committed" in withManager { manager =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner    = Miner.StrictDisabledMiner
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc) ++ Seq(
          AddrWithBalance(
            thisNodeAcc.toAddress,
            MinimalEffectiveBalanceForGenerator2 + TestValues.commitToGenerationFee + CommitToGenerationTransaction.DepositInWavelets
          )
        ),
        miner = Miner.forwardTo(miner)
      ) { d =>
        d.wallet.generateNewAccounts(1)

        val minerImpl = new MinerImpl(
          channels,
          d.blockchain,
          d.settings,
          d.testTime,
          d.utxPool,
          BlockEndorser.Disabled,
          EndorsementStorage.Disabled,
          d.wallet,
          d.posSelector,
          minerScheduler,
          appenderScheduler,
          Observable.empty
        ) with CatchLogs
        miner = minerImpl

        log.debug("Append block2")
        val block2 = d.createBlock(generator = otherNodeAcc, strictTime = true)
        d.appender.appendBlock(block2)
        val lastBlockId = d.appendMicroBlock(TxHelpers.commitToGeneration(Height(3), sender = otherNodeAcc))

        log.debug("Trigger thisNode forging")
        d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.lastBlockId shouldBe lastBlockId // Not changed
      }
    }
  }

  "Finalization on miner" in withManager { manager =>
    val generator1    = thisNodeAcc
    val generator2    = TxHelpers.signer(1)
    val generator3    = TxHelpers.signer(2)
    val generator3Idx = GeneratorIndex(2)

    val otherAcc1 = TxHelpers.signer(100)
    val otherAcc2 = TxHelpers.signer(101)

    val generators = Seq(generator1, generator2, generator3)
    val initBalances = Seq(
      AddrWithBalance(generator1.toAddress, 2000.waves), // this node miner
      AddrWithBalance(generator2.toAddress, 3000.waves),
      AddrWithBalance(generator3.toAddress, 5000.waves), // endorser
      AddrWithBalance(otherAcc1.toAddress, 2000.waves)
    ).map(x => x.copy(balance = x.balance + CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee))

    val minerScheduler    = TestScheduler()
    val appenderScheduler = TestScheduler()

    val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
    var miner    = Miner.StrictDisabledMiner
    val time     = TestTime()
    withDomain(defaultSettings, initBalances, miner = Miner.forwardTo(miner), time = time) { d =>
      d.wallet.generateNewAccounts(1)

      val endorsementStorage = EndorsementStorage.InMemory((blockId, h) => blockId == d.blockchain.blockId(h.toInt))
      val blockEndorser = BlockEndorser.InMemory(d.settings.synchronizationSettings.maxRollback, d.blockchain, d.wallet, endorsementStorage, channels)
      val utxEvents     = ConcurrentSubject.publish[Unit](using minerScheduler)
      val minerImpl = new MinerImpl(
        channels,
        d.blockchain,
        d.settings,
        time,
        d.utxPool,
        blockEndorser,
        endorsementStorage,
        d.wallet,
        d.posSelector,
        minerScheduler,
        appenderScheduler,
        utxEvents
      ) with CatchLogs
      miner = minerImpl

      val genesisBlockId = d.lastBlockId

      log.debug(s"Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = otherAcc1, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      log.debug(s"Trigger forging block 3")
      time.setTimeIfGreater(d.nextBlockTime(generator1))
      appenderScheduler.tickNext("appender-1")
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")

      log.debug(s"Trigger forging micro block 1 of block 3, reaching finalization")
      endorsementStorage.tryAdd(
        EndorseBlock(
          endorserIndex = generator3Idx.toInt,
          finalizedId = genesisBlockId,
          finalizedHeight = GenesisBlockHeight,
          endorsedId = block2WithCommitments.id(),
          signature = BlockEndorsement.sign(BlsKeyPair(generator3.privateKey), genesisBlockId, GenesisBlockHeight, block2WithCommitments.id()).byteStr
        )
      ) should beRight

      d.utxPool.putIfNew(TxHelpers.transfer(otherAcc1, otherAcc2.toAddress))
      utxEvents.onNext(())
      time.advance(1.millis)
      minerScheduler.tickNext("miner-2")
      appenderScheduler.tickNext("appender-3")

      log.debug("Append block 3 and calculate finalization")
      val block3 =
        d.createBlock(generator = generator2, strictTime = true, ref = Some(d.lastBlockId))
      d.appender.appendBlock(block3)
      d.finalizedHeightIs(2)
    }
  }

  "Correct total block signature" in withManager { manager =>
    val generator1    = TxHelpers.signer(1)
    val generator1Idx = GeneratorIndex(0)

    val generator2     = thisNodeAcc
    val generator2Addr = generator2.toAddress

    val generator3    = TxHelpers.signer(2)
    val generator3Idx = GeneratorIndex(2)

    val generators = Seq(generator1, generator2, generator3)
    val initBalances = Seq(
      AddrWithBalance(generator1.toAddress, 5000.waves),
      AddrWithBalance(generator2.toAddress, 2000.waves),
      AddrWithBalance(generator3.toAddress, 3000.waves)
    )

    val minerScheduler    = TestScheduler()
    val appenderScheduler = TestScheduler()

    val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
    var miner    = Miner.StrictDisabledMiner
    val time     = TestTime()
    withDomain(defaultSettings, initBalances, miner = Miner.forwardTo(miner), time = time) { d =>
      d.wallet.generateNewAccounts(1)

      val endorsementStorage = EndorsementStorage.InMemory((blockId, h) => blockId == d.blockchain.blockId(h.toInt))
      val blockEndorser = BlockEndorser.InMemory(d.settings.synchronizationSettings.maxRollback, d.blockchain, d.wallet, endorsementStorage, channels)
      val minerImpl = new MinerImpl(
        channels,
        d.blockchain,
        d.settings,
        time,
        d.utxPool,
        blockEndorser,
        endorsementStorage,
        d.wallet,
        d.posSelector,
        minerScheduler,
        appenderScheduler,
        Observable.empty
      ) with CatchLogs
      miner = minerImpl

      val genesisBlockId = d.lastBlockId

      log.debug(s"Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(txs, generator = generator2, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      log.debug(s"Trigger forging block 3")
      time.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
      appenderScheduler.tickNext("appender-1")
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")

      log.debug(s"Trigger forging micro block 1 of block 3, reaching finalization")
      endorsementStorage.tryAdd(
        EndorseBlock(
          endorserIndex = generator1Idx.toInt,
          finalizedId = genesisBlockId,
          finalizedHeight = GenesisBlockHeight,
          endorsedId = block2WithCommitments.id(),
          signature = BlockEndorsement.sign(BlsKeyPair(generator1.privateKey), genesisBlockId, GenesisBlockHeight, block2WithCommitments.id()).byteStr
        )
      ) should beRight
      d.utxPool.putIfNew(TxHelpers.transfer(generator1, generator2Addr))

      time.advance(1.millis)
      minerScheduler.tickNext("miner-2")
      appenderScheduler.tickNext("appender-3")

      log.debug(s"Trigger forging micro block 2 of block 3, losing finalization")
      val otherFinalizedBlockId = TxHelpers.randomBlockId
      endorsementStorage.tryAdd(
        EndorseBlock(
          endorserIndex = generator1Idx.toInt,
          finalizedId = otherFinalizedBlockId,
          finalizedHeight = GenesisBlockHeight,
          endorsedId = block2WithCommitments.id(),
          signature =
            BlockEndorsement.sign(BlsKeyPair(generator1.privateKey), otherFinalizedBlockId, GenesisBlockHeight, block2WithCommitments.id()).byteStr
        )
      ) should beRight
      d.utxPool.putIfNew(TxHelpers.transfer(generator1, generator2Addr))

      time.advance(defaultSettings.minerSettings.microBlockInterval + 1.millis)
      minerScheduler.tickNext("miner-3")
      appenderScheduler.tickNext("appender-4")
      val microBlock2TotalId = d.lastBlockId

      log.debug(s"Trigger forging micro block 3 of block 3, reaching finalization")
      endorsementStorage.tryAdd(
        EndorseBlock(
          endorserIndex = generator3Idx.toInt,
          finalizedId = genesisBlockId,
          finalizedHeight = GenesisBlockHeight,
          endorsedId = block2WithCommitments.id(),
          signature = BlockEndorsement.sign(BlsKeyPair(generator3.privateKey), genesisBlockId, GenesisBlockHeight, block2WithCommitments.id()).byteStr
        )
      ) should beRight
      d.utxPool.putIfNew(TxHelpers.transfer(generator1, generator2Addr))

      time.advance(defaultSettings.minerSettings.microBlockInterval + 1.millis)
      minerScheduler.tickNext("miner-4")
      appenderScheduler.tickNext("appender-5")
      val microBlock3TotalId = d.lastBlockId
      microBlock3TotalId shouldNot be(microBlock2TotalId) // Appended

      log.debug(s"Trigger forging micro block 4 of block 3 without finalization changes")
      d.utxPool.putIfNew(TxHelpers.transfer(generator1, generator2Addr))
      time.advance(defaultSettings.minerSettings.microBlockInterval + 1.millis)
      minerScheduler.tickNext("miner-5")
      appenderScheduler.tickNext("appender-6")
      withClue("appended: ") {
        d.lastBlockId shouldNot be(microBlock3TotalId)
      }
    }
  }
}
