package com.wavesplatform.finalization

import com.wavesplatform.block.{Block, FinalizationVoting}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.BlockChallengerImpl
import com.wavesplatform.network.{EndorseBlock, MessageCodec, PeerDatabase}
import com.wavesplatform.state.*
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.TestTime
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.{EmbeddedChannelOps, Schedulers}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.SchedulerService

class BlockEndorsementBroadcastSpec extends BaseFinalizationSpec, EmbeddedChannelOps {
  private val appenderScheduler: SchedulerService = Schedulers.singleThread("appender")
  private val testTime: TestTime                  = TestTime()

  private val sender = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)

  private val defaultSettings = DomainPresets.DeterministicFinality
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .configure(
      _.copy(
        generationPeriodLength = 3,
        lightNodeBlockFieldsAbsenceInterval = 0
      )
    )

  private val generator1 = sender
  private val generator2 = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 1)

  "should not broadcast a block endorsement" - {
    "before the feature activation" in withManager { manager =>
      withDomain(DomainPresets.TransactionStateSnapshot, AddrWithBalance.enoughBalances(sender)) { d =>
        val blockChallenger = new BlockChallengerImpl(
          d.blockchain,
          new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
          d.wallet,
          d.settings,
          testTime,
          d.posSelector,
          _ => throw new RuntimeException("Unexpected call in block challenger")
        )

        val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
        val channel1 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        val channel2 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        channels.add(channel1)
        channels.add(channel2)
        val appender = BlockAppender(
          d.blockchain,
          testTime,
          d.utxPool,
          d.posSelector,
          channels,
          PeerDatabase.NoOp,
          Some(blockChallenger),
          d.createBlockEndorser(channels),
          appenderScheduler
        )(channel2, _, None)

        val block = d.createBlock(generator = sender, strictTime = true)

        testTime.setTime(block.header.timestamp)
        appender(block).runSyncUnsafe()

        channel1.sentEndorsements.length shouldBe 0
      }
    }

    "if miner" in withManager { manager =>
      testWithGenerator { d =>
        val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
        val channel1 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        val channel2 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        channels.add(channel1)
        channels.add(channel2)

        val appender = BlockAppender(
          d.blockchain,
          testTime,
          d.utxPool,
          d.posSelector,
          channels,
          PeerDatabase.NoOp,
          blockChallenger = None,
          d.createBlockEndorser(channels, new EndorsementStorage.InMemory((_, _) => true)),
          appenderScheduler
        )(channel2, _, None)

        val block2 = d.createBlock(generator = generator1, strictTime = true)
        testTime.setTime(block2.header.timestamp)
        appender(block2).runSyncUnsafe()
        if (d.lastBlockId != block2.id()) fail(s"Can't apply endorsedBlock $block2, see logs")
        channel1.sentEndorsements.length shouldBe 0

        val block3 = d.createBlock(generator = generator2, strictTime = true)
        testTime.setTime(block3.header.timestamp)
        appender(block3).runSyncUnsafe()
        if (d.lastBlockId != block3.id()) fail(s"Can't apply block3 $block3, see logs")
        channel1.sentEndorsements.length shouldBe 0

        val block4 = d.createBlock(generator = generator1, strictTime = true)
        testTime.setTime(block4.header.timestamp)
        appender(block4).runSyncUnsafe()
        if (d.lastBlockId != block4.id()) fail(s"Can't apply block4 $block4, see logs")
        channel1.sentEndorsements.length shouldBe 0
      }
    }

    "if endorsed block is finalized already" in withManager { manager =>
      val otherGenerator = Wallet.generateNewAccount(Domain.DefaultWalletSeed :+ 1.toByte, nonce = 0)
      withDomain(
        defaultSettings
          .copy(synchronizationSettings = defaultSettings.synchronizationSettings)
          .configure(_.copy(generationPeriodLength = 3)),
        AddrWithBalance.enoughBalances(generator1, otherGenerator)
      ) { d =>
        d.wallet.generateNewAccounts(1)
        val genesisBlockId = d.lastBlockId

        val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
        val channel1 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        val channel2 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        channels.add(channel1)

        val appender = BlockAppender(
          d.blockchain,
          testTime,
          d.utxPool,
          d.posSelector,
          channels,
          PeerDatabase.NoOp,
          blockChallenger = None,
          d.createBlockEndorser(channels, new EndorsementStorage.InMemory((_, _) => true)),
          appenderScheduler
        )(channel2, _, None)

        d.appendBlock(d.createBlock(generator = generator1, strictTime = true))
        val block3 = d.createBlock(
          txs = Seq(generator1, otherGenerator).map(TxHelpers.commitToGeneration(Height(4), _)),
          generator = generator1,
          strictTime = true
        )
        d.appendBlock(block3)

        def appendBlock(n: Int, finalizationVoting: Option[FinalizationVoting] = None): Block = {
          val r = d.createBlock(generator = otherGenerator, strictTime = true, finalizationVoting = finalizationVoting)
          testTime.setTime(r.header.timestamp)
          appender(r).runSyncUnsafe()
          if (d.lastBlockId != r.id()) fail(s"Can't apply block$n $r, see logs")
          r
        }

        appendBlock(
          n = 4,
          Some(
            mkFinalizationVoting(valid = Seq(0).map(GeneratorIndex(_)))
              .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = generator1)
          )
        )

        appendBlock(5)
        d.blockchain.finalizedHeight.value shouldBe Height(3)

        log.debug("Rollback to finalized block")
        d.blockchain.removeAfter(block3.id())
        channel1.outboundMessages().clear()

        log.debug("Append new block 4")
        appendBlock(4)
        d.blockchain.finalizedHeight.value shouldBe Height(3)
        channel1.sentEndorsements shouldBe empty
      }
    }
  }

  "should broadcast a block endorsement if validator" in withManager { manager =>
    val otherGenerator = Wallet.generateNewAccount(Domain.DefaultWalletSeed :+ 1.toByte, nonce = 0)
    withDomain(defaultSettings, AddrWithBalance.enoughBalances(generator1, otherGenerator)) { d =>
      d.wallet.generateNewAccounts(1)

      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
      val channel1 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
      val channel2 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
      channels.add(channel1)
      channels.add(channel2)

      val appender = BlockAppender(
        d.blockchain,
        testTime,
        d.utxPool,
        d.posSelector,
        channels,
        PeerDatabase.NoOp,
        blockChallenger = None,
        d.createBlockEndorser(channels, new EndorsementStorage.InMemory((_, _) => true)),
        appenderScheduler
      )(channel2, _, None)

      d.appendBlock(d.createBlock(generator = generator1, strictTime = true))
      d.appendBlock(
        d.createBlock(
          txs = Seq(generator1, otherGenerator).map(TxHelpers.commitToGeneration(Height(4), _)),
          generator = generator1,
          strictTime = true
        )
      )

      val endorsedBlock = d.createBlock(generator = generator1, strictTime = true)
      testTime.setTime(endorsedBlock.header.timestamp)
      appender(endorsedBlock).runSyncUnsafe()
      if (d.lastBlockId != endorsedBlock.id()) fail(s"Can't apply endorsedBlock $endorsedBlock, see logs")
      channel1.sentEndorsements.length shouldBe 0

      val nextBlock = d.createBlock(generator = otherGenerator, strictTime = true)
      testTime.setTime(nextBlock.header.timestamp)
      appender(nextBlock).runSyncUnsafe()
      if (d.lastBlockId != nextBlock.id()) fail(s"Can't apply nextBlock $nextBlock, see logs")
      channel1.sentEndorsements.length shouldBe 1
    }
  }

  "expected endorsement fields" - {
    "voting height increased if surpass maxRollback blocks" in withManager { manager =>
      val otherGenerator = Wallet.generateNewAccount(Domain.DefaultWalletSeed :+ 1.toByte, nonce = 0)
      withDomain(
        defaultSettings.copy(synchronizationSettings = defaultSettings.synchronizationSettings.copy(maxRollback = 2)),
        AddrWithBalance.enoughBalances(generator1, otherGenerator)
      ) { d =>
        d.wallet.generateNewAccounts(1)

        val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
        val channel1 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        val channel2 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        channels.add(channel1)

        val appender = BlockAppender(
          d.blockchain,
          testTime,
          d.utxPool,
          d.posSelector,
          channels,
          PeerDatabase.NoOp,
          blockChallenger = None,
          d.createBlockEndorser(channels, new EndorsementStorage.InMemory((_, _) => true)),
          appenderScheduler
        )(channel2, _, None)

        d.appendBlock(d.createBlock(generator = generator1, strictTime = true))
        val block3 = d.createBlock(
          txs = Seq(generator1, otherGenerator).map(TxHelpers.commitToGeneration(Height(4), _)),
          generator = generator1,
          strictTime = true
        )
        d.appendBlock(block3)

        def appendBlock(n: Int): Block = {
          val r = d.createBlock(generator = otherGenerator, strictTime = true)
          testTime.setTime(r.header.timestamp)
          appender(r).runSyncUnsafe()
          if (d.lastBlockId != r.id()) fail(s"Can't apply block$n $r, see logs")
          r
        }

        val block4 = appendBlock(4)
        channel1.outboundMessages().clear()

        appendBlock(5)
        d.blockchain.finalizedHeight.value shouldBe Height(2) // Not 3!

        val endorsement = channel1.sentEndorsements.head.copy(endorserIndex = -1, signature = ByteStr.empty)
        endorsement shouldBe EndorseBlock(
          finalizedId = block3.id(),
          finalizedHeight = Height(3), // votingHeight - maxRollback, where votingHeight is 5
          endorsedId = block4.id(),
          endorserIndex = -1,
          signature = ByteStr.empty
        )
      }
    }

    "no prior finalization, finalization height in endorsement is 1" in withManager { manager =>
      val otherGenerator = Wallet.generateNewAccount(Domain.DefaultWalletSeed :+ 1.toByte, nonce = 0)
      withDomain(
        defaultSettings.copy(synchronizationSettings = defaultSettings.synchronizationSettings),
        AddrWithBalance.enoughBalances(generator1, otherGenerator)
      ) { d =>
        d.wallet.generateNewAccounts(1)
        val genesisBlockId = d.lastBlockId

        val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
        val channel1 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        val channel2 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        channels.add(channel1)

        val appender = BlockAppender(
          d.blockchain,
          testTime,
          d.utxPool,
          d.posSelector,
          channels,
          PeerDatabase.NoOp,
          blockChallenger = None,
          d.createBlockEndorser(channels, new EndorsementStorage.InMemory((_, _) => true)),
          appenderScheduler
        )(channel2, _, None)

        d.appendBlock(d.createBlock(generator = generator1, strictTime = true))
        d.appendBlock(
          d.createBlock(
            txs = Seq(generator1, otherGenerator).map(TxHelpers.commitToGeneration(Height(4), _)),
            generator = generator1,
            strictTime = true
          )
        )

        def appendBlock(n: Int): Block = {
          val r = d.createBlock(generator = otherGenerator, strictTime = true)
          testTime.setTime(r.header.timestamp)
          appender(r).runSyncUnsafe()
          if (d.lastBlockId != r.id()) fail(s"Can't apply block$n $r, see logs")
          r
        }

        val block4 = appendBlock(4)
        channel1.outboundMessages().clear()

        appendBlock(5)
        d.blockchain.finalizedHeight.value shouldBe Height(1)

        val endorsement = channel1.sentEndorsements.head.copy(endorserIndex = -1, signature = ByteStr.empty)
        endorsement shouldBe EndorseBlock(
          finalizedId = genesisBlockId,
          finalizedHeight = Height(1),
          endorsedId = block4.id(),
          endorserIndex = -1,
          signature = ByteStr.empty
        )
      }
    }

    "finalized before, finalization height in endorsement is 3" in withManager { manager =>
      val otherGenerator = Wallet.generateNewAccount(Domain.DefaultWalletSeed :+ 1.toByte, nonce = 0)
      withDomain(
        defaultSettings
          .copy(synchronizationSettings = defaultSettings.synchronizationSettings)
          .configure(_.copy(generationPeriodLength = 3)),
        AddrWithBalance.enoughBalances(generator1, otherGenerator)
      ) { d =>
        d.wallet.generateNewAccounts(1)
        val genesisBlockId = d.lastBlockId

        val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
        val channel1 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        val channel2 = manager(new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp)))
        channels.add(channel1)

        val appender = BlockAppender(
          d.blockchain,
          testTime,
          d.utxPool,
          d.posSelector,
          channels,
          PeerDatabase.NoOp,
          blockChallenger = None,
          d.createBlockEndorser(channels, new EndorsementStorage.InMemory((_, _) => true)),
          appenderScheduler
        )(channel2, _, None)

        d.appendBlock(d.createBlock(generator = generator1, strictTime = true))
        val block3 = d.createBlock(
          txs = Seq(generator1, otherGenerator).map(TxHelpers.commitToGeneration(Height(4), _)),
          generator = generator1,
          strictTime = true
        )
        d.appendBlock(block3)

        def appendBlock(n: Int, finalizationVoting: Option[FinalizationVoting] = None): Block = {
          val r = d.createBlock(generator = otherGenerator, strictTime = true, finalizationVoting = finalizationVoting)
          testTime.setTime(r.header.timestamp)
          appender(r).runSyncUnsafe()
          if (d.lastBlockId != r.id()) fail(s"Can't apply block$n $r, see logs")
          r
        }

        appendBlock(
          n = 4,
          Some(
            mkFinalizationVoting(valid = Seq(0).map(GeneratorIndex(_)))
              .signed(endorsedId = block3.id(), finalizedId = genesisBlockId, validEndorsers = generator1)
          )
        )
        val block5 = appendBlock(5)
        channel1.outboundMessages().clear()

        appendBlock(6)
        d.blockchain.finalizedHeight.value shouldBe Height(3)

        val endorsement = channel1.sentEndorsements.head.copy(endorserIndex = -1, signature = ByteStr.empty)
        endorsement shouldBe EndorseBlock(
          finalizedId = block3.id(),
          finalizedHeight = Height(3),
          endorsedId = block5.id(),
          endorserIndex = -1,
          signature = ByteStr.empty
        )
      }
    }
  }

  private def testWithGenerator(f: Domain => Any): Any = {
    val generators = Seq(generator1, generator2)
    withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
      d.wallet.generateNewAccounts(3)

      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(4), x))
      val block2WithCommitments = d.createBlock(txs, generator = generator1, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)
      (3 to 5).foreach { _ =>
        d.appender.appendBlock(d.createBlock(generator = generator1, strictTime = true))
      }

      f(d)
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    appenderScheduler.shutdown()
  }
}
