package com.wavesplatform.state.appender

import com.wavesplatform.TestValues
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonGeneratorsApi.GeneratorEntry
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.BlockChallengerImpl
import com.wavesplatform.network.{EndorseBlockSpec, MessageCodecL1, PBBlockSpec, PeerDatabase, RawBytes}
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Ignored
import com.wavesplatform.state.{CompleteBlockchainUpdater, ForwardingBlockchainUpdaterImpl, GenerationPeriod, Height, TransactionId}
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{FreeSpec, NumericExt, TestTime}
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
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
          generatorBalances = Seq.empty
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

    def testWithGenerator(f: Domain => Any): Any = {
      def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
        private val blsKeyPair = BlsKeyPair(sender.privateKey)

        override def committedGenerators(at: GenerationPeriod): IndexedSeq[(Address, BlsPublicKey, TransactionId)] =
          IndexedSeq((sender.toAddress, blsKeyPair.publicKey, TxHelpers.randomId))
      }

      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(sender),
        wrapBU = wrapBU
      ) { d =>
        d.wallet.generateNewAccounts(1).foreach(x => require(x.toAddress == sender.toAddress))
        f(d)
      }
    }

    "should append a block if no one committed" in {
      def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
        override def committedGenerators(at: GenerationPeriod): IndexedSeq[(Address, BlsPublicKey, TransactionId)] = IndexedSeq.empty
      }

      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(sender),
        wrapBU = wrapBU
      ) { d =>
        d.wallet.generateNewAccounts(1).foreach(x => require(x.toAddress == sender.toAddress))

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
        d.appender.appendBlock(block)

        d.blockchain.isLastBlockId(block.id()) shouldBe true
      }
    }

    "should append a block if committed" in testWithGenerator { d =>
      val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
      d.appender.appendBlock(block)

      d.blockchain.isLastBlockId(block.id()) shouldBe true
    }

    "should append a block if no one eligible committed" in testWithGenerator { d =>
      val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
      d.appender.appendBlock(block)

      d.blockchain.isLastBlockId(block.id()) shouldBe true
    }

    "should reject a block if not committed" in {
      val generator = Wallet.generateNewAccount(seed.arr, nonce = 1)

      def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
        private val blsKeyPair = BlsKeyPair(generator.privateKey)

        override def committedGenerators(at: GenerationPeriod): IndexedSeq[(Address, BlsPublicKey, TransactionId)] =
          IndexedSeq((generator.toAddress, blsKeyPair.publicKey, TxHelpers.randomId))
      }

      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(sender, generator),
        wrapBU = wrapBU
      ) { d =>
        d.wallet.generateNewAccounts(1).foreach(x => require(x.toAddress == sender.toAddress))

        val block = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = sender, strictTime = true)
        d.appender.appendBlock(block, requireAppended = false)

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
      val miner1InitBalance = 100_000.waves + CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee
      val miner2InitBalance = 50_000.waves + CommitToGenerationTransaction.DepositInWavelets + TestValues.commitToGenerationFee

      val miner1 = Wallet.generateNewAccount(seed.arr, nonce = 1)
      val miner2 = Wallet.generateNewAccount(seed.arr, nonce = 2)

      withDomain(
        defaultSettings.configure(_.copy(generationPeriod = 3)),
        Seq(
          sender -> 10_000.waves,
          miner1 -> miner1InitBalance,
          miner2 -> miner2InitBalance
        )
      ) { d =>
        d.wallet.generateNewAccounts(3)
        val generationPeriod1 = d.blockchain.generationPeriodOf(Height(1)).next
        generationPeriod1.start shouldBe 3

        log.info("block2")
        val txs    = Seq(miner1, miner2).map(TxHelpers.commitToGeneration(generationPeriod1.start, _))
        val block2 = d.createBlock(Block.ProtoBlockVersion, txs, generator = sender, strictTime = true)
        d.appender.appendBlock(block2)

        d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod) shouldBe empty
        d.blockchain.parentGeneratorBalances() shouldBe empty
        d.blockchain.currentGeneratorBalances() shouldBe empty
        d.generatorsApi.generators(Height(d.blockchain.height)) shouldBe empty

        log.info("block3, first period with committed generators")
        val transfer = TxHelpers.transfer(miner1, miner2.toAddress, amount = 4_000.waves, fee = 1_000.waves)
        val block3   = d.createBlock(Block.ProtoBlockVersion, Seq(transfer), generator = miner2, strictTime = true)
        d.appender.appendBlock(block3)

        d.blockchain.committedGenerators(d.blockchain.currentGenerationPeriod).map { case (addr, _, _) => addr } shouldBe
          Seq(miner1, miner2).map(_.toAddress)
        d.blockchain.parentGeneratorBalances() shouldBe empty // No committed generators in a parent block

        val miner1BalanceBeforeBlock3 = miner1InitBalance - CommitToGenerationTransaction.DepositInWavelets - TestValues.commitToGenerationFee
        val miner2BalanceBeforeBlock3 = miner2InitBalance - CommitToGenerationTransaction.DepositInWavelets - TestValues.commitToGenerationFee
        d.blockchain.currentGeneratorBalances() shouldBe Seq(miner1BalanceBeforeBlock3, miner2BalanceBeforeBlock3)
        d.generatorsApi.generators(Height(d.blockchain.height)) shouldBe Seq(
          GeneratorEntry(miner1.toAddress, miner1BalanceBeforeBlock3, TransactionId(txs.head.id())),
          GeneratorEntry(miner2.toAddress, miner2BalanceBeforeBlock3, TransactionId(txs(1).id()))
        )

        log.info("block4")
        val block4 = d.createBlock(Block.ProtoBlockVersion, Seq.empty, generator = miner2, strictTime = true)
        d.appender.appendBlock(block4)

        val miner1BalanceBeforeBlock4 = miner1BalanceBeforeBlock3 - transfer.amount.value - transfer.fee.value
        val miner2BalanceBeforeBlock4 = miner2BalanceBeforeBlock3
        d.blockchain.parentGeneratorBalances() shouldBe Seq(miner1BalanceBeforeBlock3, miner2BalanceBeforeBlock3)
        d.blockchain.currentGeneratorBalances() shouldBe Seq(miner1BalanceBeforeBlock4, miner2BalanceBeforeBlock4)
        d.generatorsApi.generators(Height(d.blockchain.height)) shouldBe Seq(
          GeneratorEntry(miner1.toAddress, miner1BalanceBeforeBlock4, TransactionId(txs.head.id())),
          GeneratorEntry(miner2.toAddress, miner2BalanceBeforeBlock4, TransactionId(txs(1).id()))
        )
      }
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    appenderScheduler.shutdown()
  }
}
