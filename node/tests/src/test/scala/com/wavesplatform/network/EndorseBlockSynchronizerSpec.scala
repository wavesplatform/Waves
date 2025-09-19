package com.wavesplatform.network

import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.BlockEndorsement
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.SignatureLength
import com.wavesplatform.crypto.bls.{BlsKeyPair, BlsPublicKey}
import com.wavesplatform.network.EndorsementStorage.EndorsementFilter
import com.wavesplatform.state.Height
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.TxHelpers
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.ExecutionModel
import monix.execution.schedulers.TestScheduler
import monix.reactive.subjects.PublishSubject as PS
import org.scalatest.Ignore

import java.util.concurrent.ThreadLocalRandom
import scala.util.Using

@Ignore // TODO:
class EndorseBlockSynchronizerSpec extends FreeSpec {
  private val testScheduler = TestScheduler(ExecutionModel.AlwaysAsyncExecution)

  private val activeGenerator     = BlsKeyPair(TxHelpers.signer(0).privateKey)
  private val committedGenerator  = BlsKeyPair(TxHelpers.signer(1).privateKey)
  private val activeEndorserIndex = 1
  private val finalizedId         = mkRandomBlockId
  private val blockId             = mkRandomBlockId
  private val blockHeight         = Height(10)

  // TODO: EndorsementStorage
  // TODO: Additional tests
  "Should ignore" - {
    "an already received endorsement" in withContext { c =>
      c.blockchainUpdated(blockHeight, blockId, activeGenerator.publicKey)

      val msg = EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, blockHeight, blockId))
      c.receivedEndorseBlock(msg)
      c.outChannel.outboundMessages().poll() shouldBe msg

      c.receivedEndorseBlock(msg)
      c.outChannel.outboundMessages() shouldBe empty
    }

    "an endorsement with" - {
      // TODO: use args with default values instead
      def test(msg: EndorseBlock): Unit = withContext { c =>
        c.blockchainUpdated(blockHeight, blockId, activeGenerator.publicKey)
        c.receivedEndorseBlock(msg)
        c.outChannel.outboundMessages() shouldBe empty
      }

      "a wrong signature" in test(
        EndorseBlock(activeEndorserIndex, finalizedId, blockHeight, blockId, ByteStr.empty)
      )
      "an unexpected height" in test(
        EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, Height(Int.MaxValue), blockId))
      )
      "an unexpected endorser" in test(EndorseBlock.from(BlockEndorsement.full(committedGenerator, 2, finalizedId, blockHeight, blockId)))
      "an already finalized block" in test(
        EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, blockHeight, finalizedId))
      )
    }
  }

  "Should rebroadcast a valid endorsement on same height after a rollback" in withContext { c =>
    // TODO: blockHeight
    c.blockchainUpdated(blockHeight, blockId, activeGenerator.publicKey)

    val msg = EndorseBlock.from(BlockEndorsement.full(activeGenerator, activeEndorserIndex, finalizedId, blockHeight, blockId))
    c.receivedEndorseBlock(msg)
    c.outChannel.outboundMessages().poll()

    c.blockchainUpdated(blockHeight, mkRandomBlockId, activeGenerator.publicKey) // height - 1
    c.blockchainUpdated(blockHeight, blockId, activeGenerator.publicKey)

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

    val last         = PS[EndorsementFilter]()
    val endorsements = PS[(Channel, EndorseBlock)]()
    val storage      = EndorsementStorage.InMemory()
    val synchronizer = EndorseBlockSynchronizer.start(storage, last, endorsements, allChannels, testScheduler)

    def blockchainUpdated(finalizedHeight: Height, blockId: BlockId, newEndorsers: BlsPublicKey*): Unit = {
      last.onNext(EndorsementFilter(finalizedId, finalizedHeight, blockId, newEndorsers.toIndexedSeq)) // TODO: finalizedId
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
