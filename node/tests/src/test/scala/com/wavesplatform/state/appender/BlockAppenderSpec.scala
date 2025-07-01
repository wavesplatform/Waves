package com.wavesplatform.state.appender

import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.mining.BlockChallengerImpl
import com.wavesplatform.network.{EndorseBlockSpec, MessageCodecL1, PBBlockSpec, PeerDatabase, RawBytes}
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Ignored
import com.wavesplatform.state.{CompleteBlockchainUpdater, ForwardingBlockchainUpdaterImpl, Height}
import com.wavesplatform.test.{FlatSpec, TestTime}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.Schedulers
import com.wavesplatform.wallet.Wallet
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.SchedulerService
import org.scalatest.BeforeAndAfterAll

import scala.jdk.CollectionConverters.*

class BlockAppenderSpec extends FlatSpec with WithDomain with BeforeAndAfterAll {
  val appenderScheduler: SchedulerService = Schedulers.singleThread("appender")
  val testTime: TestTime                  = TestTime()

  behavior of "BlockAppender"

  it should "not broadcast block that wasn't applied to state" in {
    val sender = TxHelpers.signer(1)
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
          None
        )
        .explicitGet() shouldBe Ignored

      appender(block).runSyncUnsafe()
      channel1.outboundMessages().isEmpty shouldBe true
    }
  }

  it should "not broadcast block endorsement before the feature activation" in {
    val sender = TxHelpers.signer(1)
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

  it should "broadcast block endorsement after the feature activation" in {
    val seed   = ByteStr("finality-test".getBytes())
    val sender = Wallet.generateNewAccount(seed.arr, nonce = 0)

    def wrapBU(bu: CompleteBlockchainUpdater): CompleteBlockchainUpdater = new ForwardingBlockchainUpdaterImpl(bu) {
      override def activeGenerators(at: Height): Seq[PublicKey] = Seq(sender.publicKey)
    }

    withDomain(
      DomainPresets.DeterministicFinality.copy(walletSettings = DomainPresets.DeterministicFinality.walletSettings.copy(seed = Some(seed))),
      AddrWithBalance.enoughBalances(sender),
      wrapBU = wrapBU
    ) { d =>
      d.wallet.generateNewAccounts(1).foreach(x => require(x.toAddress == sender.toAddress))

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

      endorsements shouldBe 1
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    appenderScheduler.shutdown()
  }
}
