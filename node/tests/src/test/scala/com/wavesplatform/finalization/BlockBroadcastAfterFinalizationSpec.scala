package com.wavesplatform.finalization

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.BlockChallengerImpl
import com.wavesplatform.network.{EndorseBlock, EndorseBlockSpec, MessageCodec, PeerDatabase, RawBytes}
import com.wavesplatform.state.*
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, TestTime}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.wallet.Wallet
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.SchedulerService

import scala.jdk.CollectionConverters.*

// TODO: remove bu
class BlockBroadcastAfterFinalizationSpec extends BaseFinalizationSpec {
  private val appenderScheduler: SchedulerService = Schedulers.singleThread("appender")
  private val testTime: TestTime                  = TestTime()

  private val sender = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)

  private val defaultSettings = DomainPresets.DeterministicFinality
    .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
    .configure(
      _.copy(
        generationPeriodLength = 2,
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

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)

        testTime.setTime(block.header.timestamp)
        appender(block).runSyncUnsafe()

        channel1.sentEndorsementsNumber shouldBe 0
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

        val endorsedBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator1, strictTime = true)
        testTime.setTime(endorsedBlock.header.timestamp)
        appender(endorsedBlock).runSyncUnsafe()
        if (d.lastBlockId != endorsedBlock.id()) fail(s"Can't apply endorsedBlock $endorsedBlock, see logs")
        channel1.sentEndorsementsNumber shouldBe 0

        val nextBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator2, strictTime = true)
        testTime.setTime(nextBlock.header.timestamp)
        appender(nextBlock).runSyncUnsafe()
        if (d.lastBlockId != nextBlock.id()) fail(s"Can't apply nextBlock $nextBlock, see logs")
        channel1.sentEndorsementsNumber shouldBe 0
      }
    }
  }

  "should broadcast a block endorsement if validator" in withManager { manager =>
    val otherGenerator = Wallet.generateNewAccount(Domain.DefaultWalletSeed :+ 1.toByte, nonce = 0)

    def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
      private val xs = Vector(generator1, otherGenerator).map { g =>
        (g.toAddress -> BlsKeyPair(g.privateKey).publicKey)
      }

      override def committedGenerators(at: GenerationPeriod): IndexedSeq[(Address, BlsPublicKey)] = xs
    }

    withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(generator1, otherGenerator),
      wrapBU = wrapBU
    ) { d =>
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

      val endorsedBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator1, strictTime = true)
      testTime.setTime(endorsedBlock.header.timestamp)
      appender(endorsedBlock).runSyncUnsafe()
      if (d.lastBlockId != endorsedBlock.id()) fail(s"Can't apply endorsedBlock $endorsedBlock, see logs")
      channel1.sentEndorsementsNumber shouldBe 0

      val nextBlock = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = otherGenerator, strictTime = true)
      testTime.setTime(nextBlock.header.timestamp)
      appender(nextBlock).runSyncUnsafe()
      if (d.lastBlockId != nextBlock.id()) fail(s"Can't apply nextBlock $nextBlock, see logs")
      channel1.sentEndorsementsNumber shouldBe 1
    }
  }

  "voting height increased if surpass maxRollback blocks" in withManager { manager =>
    val otherGenerator = Wallet.generateNewAccount(Domain.DefaultWalletSeed :+ 1.toByte, nonce = 0)

    def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
      private val xs = Vector(generator1, otherGenerator).map { g =>
        (g.toAddress -> BlsKeyPair(g.privateKey).publicKey)
      }

      override def committedGenerators(at: GenerationPeriod): IndexedSeq[(Address, BlsPublicKey)] = xs
    }

    withDomain(
      defaultSettings.copy(synchronizationSettings = defaultSettings.synchronizationSettings.copy(maxRollback = 2)),
      AddrWithBalance.enoughBalances(generator1, otherGenerator),
      wrapBU = wrapBU
    ) { d =>
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

      val block2 = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = generator1, strictTime = true)
      testTime.setTime(block2.header.timestamp)
      appender(block2).runSyncUnsafe()
      if (d.lastBlockId != block2.id()) fail(s"Can't apply block2 $block2, see logs")

      val block3 = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = otherGenerator, strictTime = true)
      testTime.setTime(block3.header.timestamp)
      appender(block3).runSyncUnsafe()
      if (d.lastBlockId != block3.id()) fail(s"Can't apply block3 $block3, see logs")
      channel1.outboundMessages().clear()

      val block4 = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = otherGenerator, strictTime = true)
      testTime.setTime(block4.header.timestamp)
      appender(block4).runSyncUnsafe()
      if (d.lastBlockId != block4.id()) fail(s"Can't apply block4 $block4, see logs")

      channel1.sentEndorsement.value.finalizedHeight shouldBe Height(2) // 4 - maxRollback
    }
  }

  private def testWithGenerator(f: Domain => Any): Any = {
    val generators = Seq(generator1, generator2)
    withDomain(defaultSettings, AddrWithBalance.enoughBalances(generators*)) { d =>
      d.wallet.generateNewAccounts(3)

      val txs                   = generators.map(x => TxHelpers.commitToGeneration(generationPeriodStart = Height(3), x))
      val block2WithCommitments = d.createBlock(version = Block.ProtoBlockVersion, txs = txs, generator = generator1, strictTime = true)
      d.appender.appendBlock(block2WithCommitments)
      (3 to 5).foreach { _ =>
        d.appender.appendBlock(d.createBlock(version = Block.ProtoBlockVersion, txs = Nil, generator = generator1, strictTime = true))
      }

      f(d)
    }
  }

  extension (self: EmbeddedChannel) {
    def sentEndorsement: Option[EndorseBlock] = {
      val xs = self.outboundMessages().asScala.collect {
        case x: RawBytes if x.code == EndorseBlockSpec.messageCode => EndorseBlockSpec.deserializeData(x.data).get
      }
      self.outboundMessages().clear()

      withClue("only one endorsement: ") {
        xs.size should be <= 1
      }
      xs.headOption
    }

    def sentEndorsementsNumber: Long = {
      val r = self.outboundMessages().asScala.count {
        case x: RawBytes if x.code == EndorseBlockSpec.messageCode => true
        case _                                                     => false
      }
      self.outboundMessages().clear()
      r
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    appenderScheduler.shutdown()
  }
}
