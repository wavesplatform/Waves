package com.wavesplatform.state.appender

import com.wavesplatform.TestValues
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.BlockChallengerImpl
import com.wavesplatform.network.{EndorseBlockSpec, MessageCodecL1, PBBlockSpec, PeerDatabase, RawBytes}
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Ignored
import com.wavesplatform.state.{CompleteBlockchainUpdater, ForwardingBlockchainUpdaterImpl, GenerationPeriod, Height}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt, TestTime}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.wallet.Wallet
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.SchedulerService
import org.scalatest.BeforeAndAfterAll

import scala.jdk.CollectionConverters.*

class BlockAppenderSpec extends FreeSpec with WithDomain with BeforeAndAfterAll {
  private val appenderScheduler: SchedulerService = Schedulers.singleThread("appender")
  private val testTime: TestTime                  = TestTime()

  private val seed   = ByteStr("finality-test".getBytes())
  private val sender = Wallet.generateNewAccount(seed.arr, nonce = 0)

  "should not broadcast block that wasn't applied to state" in {
    withDomain(DomainPresets.ConsensusImprovements, AddrWithBalance.enoughBalances(sender)) { d =>
      val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val channel1 = new EmbeddedChannel(new MessageCodecL1(PeerDatabase.NoOp))
      val channel2 = new EmbeddedChannel(new MessageCodecL1(PeerDatabase.NoOp))
      channels.add(channel1)
      channels.add(channel2)
      val appender = BlockAppender(
        d.blockchain,
        testTime,
        d.utxPool,
        d.posSelector,
        channels,
        PeerDatabase.NoOp,
        None,
        appenderScheduler
      )(channel2, _, None)

      val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)

      testTime.setTime(block.header.timestamp)
      appender(block).runSyncUnsafe()

      channel1.outboundMessages().isEmpty shouldBe false
      PBBlockSpec.deserializeData(channel1.readOutbound[RawBytes]().data).get shouldBe block

      d.blockchainUpdater
        .processBlock(
          block,
          com.wavesplatform.crypto
            .verifyVRF(block.header.generationSignature, d.blockchain.hitSource(1).get.arr, block.sender)
            .explicitGet(),
          snapshot = None,
          generatorBalances = Map.empty
        )
        .explicitGet() shouldBe Ignored

      appender(block).runSyncUnsafe()
      channel1.outboundMessages().isEmpty shouldBe true
    }
  }

  "should not broadcast a block endorsement before the feature activation" in {
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

      val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val channel1 = new EmbeddedChannel(new MessageCodecL1(PeerDatabase.NoOp))
      val channel2 = new EmbeddedChannel(new MessageCodecL1(PeerDatabase.NoOp))
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
        appenderScheduler
      )(channel2, _, None)

      val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)

      testTime.setTime(block.header.timestamp)
      appender(block).runSyncUnsafe()

      val endorsements = channel1.outboundMessages().asScala.count {
        case x: RawBytes if x.code == EndorseBlockSpec.messageCode => true
        case _                                                     => false
      }

      endorsements shouldBe 0
    }
  }

  "when DeterministicFinality activated" - {
    val defaultSettings =
      DomainPresets.DeterministicFinality.copy(walletSettings = DomainPresets.DeterministicFinality.walletSettings.copy(seed = Some(seed)))

    def mkDefaultAppender(d: Domain)(b: Block): Unit = {
      val blockChallenger = new BlockChallengerImpl(
        d.blockchain,
        new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
        d.wallet,
        d.settings,
        testTime,
        d.posSelector,
        _ => throw new RuntimeException("Unexpected call in block challenger")
      )

      val appender = BlockAppender(
        d.blockchain,
        testTime,
        d.utxPool,
        d.posSelector,
        new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
        PeerDatabase.NoOp,
        Some(blockChallenger),
        appenderScheduler
      )(new EmbeddedChannel(new MessageCodecL1(PeerDatabase.NoOp)), _, snapshot = None)

      appender(b).runSyncUnsafe()
      if (d.lastBlockId != b.id()) fail(s"Can't apply block $b, see logs")
    }

    def testWithGenerator(f: Domain => Any): Any = {
      def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
        private val blsKeyPair = BlsKeyPair(sender.privateKey)

        override def committedGenerators(at: GenerationPeriod): Map[BlsPublicKey, Address] = Map(blsKeyPair.publicKey -> sender.toAddress)
      }

      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(sender),
        wrapBU = wrapBU
      ) { d =>
        d.wallet.generateNewAccounts(1).foreach(x => require(x.toAddress == sender.toAddress))
      }
    }

    "should append a block if no one committed" in {
      def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
        override def committedGenerators(at: GenerationPeriod): Map[BlsPublicKey, Address] = Map.empty
      }

      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(sender),
        wrapBU = wrapBU
      ) { d =>
        d.wallet.generateNewAccounts(1).foreach(x => require(x.toAddress == sender.toAddress))
        val appender = mkDefaultAppender(d)

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
        testTime.setTime(block.header.timestamp)
        appender(block)

        d.blockchain.isLastBlockId(block.id()) shouldBe true
      }
    }

    "should append a block if committed" in testWithGenerator { d =>
      val appender = mkDefaultAppender(d)

      val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
      testTime.setTime(block.header.timestamp)
      appender(block)

      d.blockchain.isLastBlockId(block.id()) shouldBe true
    }

    "should append a block if no one eligible committed" in testWithGenerator { d =>
      val appender = mkDefaultAppender(d)

      val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
      testTime.setTime(block.header.timestamp)
      appender(block)

      d.blockchain.isLastBlockId(block.id()) shouldBe true
    }

    "should reject a block if not committed" in {
      val generator = Wallet.generateNewAccount(seed.arr, nonce = 1)

      def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
        private val blsKeyPair = BlsKeyPair(generator.privateKey)

        override def committedGenerators(at: GenerationPeriod): Map[BlsPublicKey, Address] = Map(blsKeyPair.publicKey -> generator.toAddress)
      }

      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(sender, generator),
        wrapBU = wrapBU
      ) { d =>
        d.wallet.generateNewAccounts(1).foreach(x => require(x.toAddress == sender.toAddress))
        val appender = mkDefaultAppender(d)

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
        testTime.setTime(block.header.timestamp)
        appender(block)

        d.blockchain.isLastBlockId(block.id()) shouldBe false
      }
    }

    "should broadcast a block endorsement" in testWithGenerator { d =>
      val blockChallenger = new BlockChallengerImpl(
        d.blockchain,
        new DefaultChannelGroup(GlobalEventExecutor.INSTANCE),
        d.wallet,
        d.settings,
        testTime,
        d.posSelector,
        _ => throw new RuntimeException("Unexpected call in block challenger")
      )

      val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val channel1 = new EmbeddedChannel(new MessageCodecL1(PeerDatabase.NoOp))
      val channel2 = new EmbeddedChannel(new MessageCodecL1(PeerDatabase.NoOp))
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
        appenderScheduler
      )(channel2, _, None)

      val b = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
      testTime.setTime(b.header.timestamp)
      appender(b).runSyncUnsafe()
      if (d.lastBlockId != b.id()) fail(s"Can't apply block $b, see logs")

      val endorsements = channel1.outboundMessages().asScala.count {
        case x: RawBytes if x.code == EndorseBlockSpec.messageCode => true
        case _                                                     => false
      }

      endorsements shouldBe 1
    }

    "committed generators balances" in {
      val miner1InitBalance = 100_000.waves
      val miner2InitBalance = 50_000.waves

      val miner1      = Wallet.generateNewAccount(seed.arr, nonce = 1)
      val miner1BlsPk = BlsKeyPair(miner1.privateKey).publicKey

      val miner2      = Wallet.generateNewAccount(seed.arr, nonce = 2)
      val miner2BlsPk = BlsKeyPair(miner2.privateKey).publicKey

      withDomain(
        defaultSettings.configure(_.copy(generationPeriod = 3)),
        Seq(
          sender -> 10_000.waves,
          miner1 -> (miner1InitBalance + TestValues.commitToGenerationFee),
          miner2 -> (miner2InitBalance + TestValues.commitToGenerationFee)
        )
      ) { d =>
        d.wallet.generateNewAccounts(3)
        val appender          = mkDefaultAppender(d)
        val generationPeriod1 = d.blockchain.generationPeriodOf(Height(1)).next

        info("block2")
        val block2 = d.createBlock(
          Block.ProtoBlockVersion,
          Seq(miner1, miner2).map(TxHelpers.commitToGeneration(generationPeriod1.start, _)),
          generator = sender, // Sender gets reward, miner1 and miner2 have exact generator balances
          strictTime = true
        )
        testTime.setTime(block2.header.timestamp)
        appender(block2)

        d.blockchain.parentGeneratorBalances() shouldBe empty
        d.blockchain.currentGeneratorBalances() shouldBe empty

        info("block3")
        val transfer = TxHelpers.transfer(miner1, miner2.toAddress, 5_000.waves)
        val block3   = d.createBlock(Block.ProtoBlockVersion, Seq(transfer), generator = miner2, strictTime = true)
        testTime.setTime(block3.header.timestamp)
        appender(block3)

        d.blockchain.parentGeneratorBalances() shouldBe empty
        d.blockchain.currentGeneratorBalances() shouldBe Map(
          miner1BlsPk -> miner1InitBalance,
          miner2BlsPk -> miner2InitBalance
        )

        info("block4")
        val block4 = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = miner2, strictTime = true)
        testTime.setTime(block4.header.timestamp)
        appender(block4)

        d.blockchain.parentGeneratorBalances() shouldBe Map(
          miner1BlsPk -> miner1InitBalance,
          miner2BlsPk -> miner2InitBalance
        )
        d.blockchain.currentGeneratorBalances() shouldBe Map(
          miner1BlsPk -> (miner1InitBalance - transfer.amount.value - transfer.fee.value),
          miner2BlsPk -> miner2InitBalance
        )
      }
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    appenderScheduler.shutdown()
  }
}
