package com.wavesplatform.finalization

import com.wavesplatform.TestValues
import com.wavesplatform.block.{Block, BlockEndorsement}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.GeneratingBalanceProvider.MinimalEffectiveBalanceForGenerator2
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.network.EndorseBlock
import com.wavesplatform.settings.*
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{CatchLogs, FreeSpec, NumericExt, TestSchedulerOps, TestTime}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

class MinerWithFinalitySuite extends BaseFinalizationSpec, TestSchedulerOps {
  private val thisNodeAcc  = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
  private val otherNodeAcc = TxHelpers.defaultSigner

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
   .copy(minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = 100.millis))
    .configure(_.copy(generationPeriodLength = 2))

  "If account not committed, its attempt to forge doesn't stop current mining of other account on same node" ignore {}

  "Mining works on new period even" - {
    "committed after scheduled time" ignore {}

    "committed in the last block of period" in withManager { manager =>
      val channels     = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner: Miner = Miner.Disabled
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc) ++ Seq(
          AddrWithBalance(
            thisNodeAcc.toAddress,
            MinimalEffectiveBalanceForGenerator2 + TestValues.commitToGenerationFee + CommitToGenerationTransaction.DepositInWavelets
          )
        ),
        miner = x => miner.scheduleMining(x)
      ) { d =>
        val minerScheduler    = TestScheduler()
        val appenderScheduler = TestScheduler()

        d.wallet.generateNewAccounts(1).map(_.toAddress)

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
        val block2 = d.createBlock(version = Block.ProtoBlockVersion, txs = Seq.empty, generator = otherNodeAcc, strictTime = true)
        d.appender.appendBlock(block2)
        d.appendMicroBlock(TxHelpers.commitToGeneration(Height(3), sender = thisNodeAcc))
        d.utxPool.cleanUnconfirmed()

        log.debug("Trigger thisNode forging")
        val nextBlockIn = (d.nextBlockTime(thisNodeAcc) - d.testTime.getTimestamp()).millis
        d.testTime.advance(nextBlockIn)
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.blockchain.lastBlockHeader.value.header.generator.toAddress shouldBe thisNodeAcc.toAddress
        minerImpl.inMemoryLog.getMessages.find(_.contains("is not committed on 3")) shouldBe empty
      }
    }

    // TODO:
    "all generators have no right to mine" - {
      "some conflict, some have no required balance" ignore {}

      "all have no required balance" ignore {}
    }

    "was conflict in previous period" in withManager { manager =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      val channels     = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner: Miner = Miner.Disabled
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc) ++ Seq(
          AddrWithBalance(
            thisNodeAcc.toAddress,
            MinimalEffectiveBalanceForGenerator2 + TestValues.commitToGenerationFee + CommitToGenerationTransaction.DepositInWavelets
          )
        ),
        miner = x => miner.scheduleMining(x)
      ) { d =>
        d.wallet.generateNewAccounts(1).map(_.toAddress)

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
        val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = otherNodeAcc, strictTime = true)
        d.appender.appendBlock(block2WithCommitments)

        log.debug("Append block3 with conflict")
        val block3WithVotes = d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = otherNodeAcc,
          strictTime = true,
          finalizationVoting = Some(mkFinalizationVoting().withConflict(thisNodeAcc, GeneratorIndex(1), block2WithCommitments.id()))
        )
        d.appender.appendBlock(block3WithVotes)

        log.debug("Append empty block")
        d.appender.appendBlock(d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = otherNodeAcc, strictTime = true))
        val block5Id = d.blockchain.lastBlockId.value

        log.debug("Trigger thisNode forging")
        val nextBlockIn = (d.nextBlockTime(thisNodeAcc) - d.testTime.getTimestamp()).millis
        d.testTime.advance(nextBlockIn)
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.blockchain.lastBlockId.value should not be block5Id
      }
    }
  }

  "Mining doesn't work" - {
    "if conflict" in withManager { manager =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      val channels     = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner: Miner = Miner.Disabled
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc) ++ Seq(
          AddrWithBalance(
            thisNodeAcc.toAddress,
            MinimalEffectiveBalanceForGenerator2 + TestValues.commitToGenerationFee + CommitToGenerationTransaction.DepositInWavelets
          )
        ),
        miner = x => miner.scheduleMining(x)
      ) { d =>
        d.wallet.generateNewAccounts(1).map(_.toAddress)

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
        val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = otherNodeAcc, strictTime = true)
        d.appender.appendBlock(block2WithCommitments)

        log.debug("Append block3 with conflict")
        val block3WithVotes = d.createBlock(
          version = Block.ProtoBlockVersion,
          txs = Nil,
          generator = otherNodeAcc,
          strictTime = true,
          finalizationVoting = Some(mkFinalizationVoting().withConflict(thisNodeAcc, GeneratorIndex(1), block2WithCommitments.id()))
        )
        d.appender.appendBlock(block3WithVotes)

        log.debug("Trigger thisNode forging")
        val nextBlockIn = (d.nextBlockTime(thisNodeAcc) - d.testTime.getTimestamp()).millis
        d.testTime.advance(nextBlockIn)
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.blockchain.lastBlockId.value shouldBe block3WithVotes.id() // Not changed
        minerImpl.inMemoryLog.getMessages.find(_.contains("is conflict on 4")) should not be empty
      }
    }

    "on new period if not committed" in withManager { manager =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      val channels     = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      var miner: Miner = Miner.Disabled
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(otherNodeAcc) ++ Seq(
          AddrWithBalance(
            thisNodeAcc.toAddress,
            MinimalEffectiveBalanceForGenerator2 + TestValues.commitToGenerationFee + CommitToGenerationTransaction.DepositInWavelets
          )
        ),
        miner = x => miner.scheduleMining(x)
      ) { d =>
        d.wallet.generateNewAccounts(1).map(_.toAddress)

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
        val block2 = d.createBlock(version = Block.ProtoBlockVersion, txs = Seq.empty, generator = otherNodeAcc, strictTime = true)
        d.appender.appendBlock(block2)
        val lastBlockId = d.appendMicroBlock(TxHelpers.commitToGeneration(Height(3), sender = otherNodeAcc))

        log.debug("Trigger thisNode forging")
        val nextBlockIn = (d.nextBlockTime(thisNodeAcc) - d.testTime.getTimestamp()).millis
        d.testTime.advance(nextBlockIn)
        appenderScheduler.tickNext("appender-1")
        minerScheduler.tickNext("miner-1")
        appenderScheduler.tickNext("appender-2")

        d.blockchain.lastBlockId.value shouldBe lastBlockId // Not changed
        minerImpl.inMemoryLog.getMessages.find(_.contains("is not committed on 3")) should not be empty
      }
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

    val channels     = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
    var miner: Miner = Miner.Disabled
    val time         = TestTime()
    withDomain(defaultSettings, initBalances, miner = x => miner.scheduleMining(x), time = time) { d =>
      d.wallet.generateNewAccounts(1).map(_.toAddress)

      val endorsementStorage = EndorsementStorage.InMemory((blockId, h) => blockId == d.blockchain.blockId(h.toInt))
      val blockEndorser      = BlockEndorser.InMemory(d.blockchain, d.wallet, endorsementStorage, channels)
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

      val genesisBlockId = d.blockchain.lastBlockId.value

      log.debug(s"Append block 2 with commitments")
      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = generator2, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)

      log.debug(s"Trigger forging block 3")
      time.advance((d.nextBlockTime(thisNodeAcc) - d.testTime.getTimestamp()).millis)
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
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")

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
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")
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
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")
      val microBlock3TotalId = d.lastBlockId

      microBlock2TotalId shouldNot be(microBlock3TotalId) // Appended
    }
  }
}
