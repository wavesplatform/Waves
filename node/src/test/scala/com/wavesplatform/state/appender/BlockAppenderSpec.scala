package com.wavesplatform.state.appender

import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.network.{MessageCodec, PBBlockSpec, PeerDatabase, RawBytes}
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Ignored
import com.wavesplatform.test.{FlatSpec, TestTime}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.Schedulers
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler.Implicits.global
import monix.execution.schedulers.SchedulerService
import org.scalatest.BeforeAndAfterAll

class BlockAppenderSpec extends FlatSpec with WithDomain with BeforeAndAfterAll {

  val appenderScheduler: SchedulerService = Schedulers.singleThread("appender")
  val testTime: TestTime                  = TestTime()

  "BlockAppender" should "not broadcast block that wasn't applied to state" in {
    val sender = TxHelpers.signer(1)
    withDomain(DomainPresets.ConsensusImprovements, AddrWithBalance.enoughBalances(sender)) { d =>
      val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val channel1 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
      val channel2 = new EmbeddedChannel(new MessageCodec(PeerDatabase.NoOp))
      channels.add(channel1)
      channels.add(channel2)
      val appender = BlockAppender(
        d.blockchain,
        testTime,
        d.utxPool,
        d.posSelector,
        channels,
        PeerDatabase.NoOp,
        appenderScheduler
      )(channel2, _)

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
            .explicitGet()
        )
        .explicitGet() shouldBe Ignored

      appender(block).runSyncUnsafe()
      channel1.outboundMessages().isEmpty shouldBe true
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    appenderScheduler.shutdown()
  }
}
