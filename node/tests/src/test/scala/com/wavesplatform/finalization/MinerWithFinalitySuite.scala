package com.wavesplatform.finalization

import com.wavesplatform.TestValues
import com.wavesplatform.block.{Block, BlockEndorsement, FinalizationVoting}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.GeneratingBalanceProvider.MinimalEffectiveBalanceForGenerator2
import com.wavesplatform.crypto.bls.BlsKeyPair
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.settings.*
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{CatchLogs, FreeSpec, TestSchedulerOps}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.EitherValues
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

import scala.util.Using

class MinerWithFinalitySuite extends FreeSpec with WithDomain with TestSchedulerOps with EitherValues {
  private val seed         = ByteStr("finality-test".getBytes())
  private val thisNodeAcc  = Wallet.generateNewAccount(seed.arr, nonce = 0)
  private val otherNodeAcc = TxHelpers.defaultSigner

  private val baseSettings = DomainPresets.DeterministicFinality.addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
  private val defaultSettings = baseSettings
    .copy(
      minerSettings = baseSettings.minerSettings.copy(quorum = 0),
      walletSettings = baseSettings.walletSettings.copy(seed = Some(seed))
    )
    .configure(_.copy(generationPeriodLength = 2))

  "If account not committed, its attempt to forge doesn't stop current mining of other account on same node" ignore {}

  "Mining works on new epoch even" - {
    "committed after scheduled time" ignore {}

    "committed in the last block of epoch" in Using.Manager { manager =>
      val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      manager.acquire(channels)(using _.close())

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
    }.get

    // TODO:
    "all generators have no right to mine" - {
      "some conflict, some have no required balance" ignore {}

      "all have no required balance" ignore {}
    }

    "was conflict in previous epoch" ignore {}
  }

  // TODO: Move to conflict
  "Mining doesn't work if conflict" in Using.Manager { manager =>
    val minerScheduler    = TestScheduler()
    val appenderScheduler = TestScheduler()

    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    manager.acquire(channels)(using _.close())

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
      val otherFinalizedBlockId = TxHelpers.randomBlockId
      val block3WithVotes = d.createBlock(
        version = Block.ProtoBlockVersion,
        txs = Nil,
        generator = otherNodeAcc,
        strictTime = true,
        finalizationVoting = Some(
          FinalizationVoting(
            conflict = Vector(
              BlockEndorsement.signed(
                BlsKeyPair(thisNodeAcc.privateKey),
                GeneratorIndex(1),
                otherFinalizedBlockId,
                finalizedHeight = GenesisBlockHeight,
                endorsedId = block2WithCommitments.id()
              )
            )
          )
        )
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
  }.get

  "Mining doesn't work on new epoch if not committed" in Using.Manager { manager =>
    val minerScheduler    = TestScheduler()
    val appenderScheduler = TestScheduler()

    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    manager.acquire(channels)(using _.close())

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
  }.get
}
