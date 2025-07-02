package com.wavesplatform.network

import com.wavesplatform.account.PublicKey
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.state.Height
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.channel.{Channel, ChannelId}
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.ExecutionModel
import monix.execution.schedulers.TestScheduler
import monix.reactive.subjects.PublishSubject as PS

import java.util.concurrent.ThreadLocalRandom
import scala.util.Using

class EndorseBlockSynchronizerSpec extends FreeSpec {
  private val testScheduler = TestScheduler(ExecutionModel.AlwaysAsyncExecution)

  private val activeGenerator    = TxHelpers.signer(0)
  private val committedGenerator = TxHelpers.signer(1)
  private val blockId            = mkRandomBlockId
  private val blockHeight        = Height(10)

  "Should ignore" - {
    "an already received endorsement" in withContext { c =>
      c.blockchainUpdated(blockHeight, activeGenerator.publicKey)

      val msg = EndorseBlock.from(BlockEndorsement.full(activeGenerator, blockId, blockHeight))
      c.receivedEndorseBlock(msg)
      c.outChannel.outboundMessages().poll() shouldBe msg

      c.receivedEndorseBlock(msg)
      c.outChannel.outboundMessages() shouldBe empty
    }

    "an endorsement with" - {
      def test(msg: EndorseBlock): Unit = withContext { c =>
        c.blockchainUpdated(blockHeight, activeGenerator.publicKey)
        c.receivedEndorseBlock(msg)
        c.outChannel.outboundMessages() shouldBe empty
      }

      "a wrong signature" in test(EndorseBlock(activeGenerator.publicKey, blockId, blockHeight, ByteStr.empty))
      "an unexpected height" in test(EndorseBlock.from(BlockEndorsement.full(activeGenerator, blockId, Height(Int.MaxValue))))
      "an unexpected endorser" in test(EndorseBlock.from(BlockEndorsement.full(committedGenerator, blockId, blockHeight)))
    }
  }

  "Should rebroadcast a valid endorsement on same height after a rollback" in withContext { c =>
    c.blockchainUpdated(blockHeight, activeGenerator.publicKey)

    val msg = EndorseBlock.from(BlockEndorsement.full(activeGenerator, blockId, blockHeight))
    c.receivedEndorseBlock(msg)
    c.outChannel.outboundMessages().poll()

    c.blockchainUpdated(blockHeight - 1, activeGenerator.publicKey)
    c.blockchainUpdated(blockHeight, activeGenerator.publicKey)

    // TODO: this should not pass
    c.receivedEndorseBlock(msg)
    c.outChannel.outboundMessages().poll() shouldBe msg
  }

  private def withContext(f: TestContext => Unit): Unit = Using(new TestContext)(f).get

  private class TestContext extends AutoCloseable {
    val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val inChannel   = new EmbeddedChannel(TestChannelId("in"))
    val outChannel  = new EmbeddedChannel(TestChannelId("out"))
    allChannels.add(inChannel)
    allChannels.add(outChannel)

    val last         = PS[EndorseBlockSynchronizer.EndorsersAt]()
    val endorsements = PS[(Channel, EndorseBlock)]()
    val synchronizer = EndorseBlockSynchronizer.start(1, last, endorsements, allChannels)(testScheduler)

    def blockchainUpdated(height: Int, newEndorsers: PublicKey*): Unit = {
      last.onNext((Height(height), newEndorsers.toSet))
      testScheduler.tick()
    }

    def receivedEndorseBlock(endorseBlock: EndorseBlock): Unit = {
      endorsements.onNext((inChannel, endorseBlock))
      testScheduler.tick()
    }

    override def close(): Unit = {
      synchronizer.cancel()
      endorsements.onComplete()
      last.onComplete()
      allChannels.close()
    }
  }

  private def mkRandomBlockId: BlockId = ByteStr(Array.fill(SignatureLength)(ThreadLocalRandom.current().nextInt(Byte.MaxValue).toByte))
}
