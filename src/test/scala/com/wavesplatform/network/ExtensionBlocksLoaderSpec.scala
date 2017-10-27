package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.ByteStr
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.NgHistory

import scala.concurrent.duration.DurationInt
import scala.util.Random

class ExtensionBlocksLoaderSpec extends FreeSpec
  with Matchers
  with MockFactory
  with Eventually
  with TransactionGen {

  private val lastCommonSignature: ByteStr = TestBlock.randomSignature()
  private val remoteExtensionBlocks: Seq[Block] = (1 to 3).scanLeft(createBlock(lastCommonSignature)) { (r, _) =>
    createBlock(r.uniqueId)
  }
  private val remoteExtensionSignatures: Seq[ByteStr] = remoteExtensionBlocks.map(_.uniqueId)

  "should request blocks" in {
    val history = mock[NgHistory]
    (history.heightOf(_: ByteStr)).expects(*)
      .onCall { _: ByteStr => None }
      .repeat(remoteExtensionBlocks.size)

    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, history))
    channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
    channel.flushInbound()

    var rest = remoteExtensionSignatures.map(GetBlock).toSet
    eventually {
      val actual = channel.readOutbound[GetBlock]()
      Option(actual).foreach { rest -= _ }
      rest shouldBe empty
    }
  }

  "should verify that blocks form a chain" in {
    val remoteExtensionBlocks = Seq(createBlock(lastCommonSignature), createBlock(TestBlock.randomSignature()))
    val remoteExtensionSignatures = remoteExtensionBlocks.map(_.uniqueId)

    val history = mock[NgHistory]
    (history.heightOf(_: ByteStr)).expects(*)
      .onCall { _: ByteStr => None }
      .repeat(remoteExtensionBlocks.size)

    var senderWasBlacklisted = false
    val peerDatabase = new PeerDatabase.NoOp {
      override def blacklistAndClose(channel: Channel, reason: String): Unit = {
        senderWasBlacklisted = true
      }
    }

    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, peerDatabase, history))
    channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
    remoteExtensionBlocks.foreach(channel.writeInbound(_))
    channel.flushInbound()

    eventually {
      senderWasBlacklisted shouldBe true
    }
  }

  "should propagate an extension after receiving all blocks" in {
    val history = mock[NgHistory]
    (history.heightOf(_: ByteStr)).expects(*)
      .onCall { _: ByteStr => None }
      .repeat(remoteExtensionBlocks.size)

    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, history))
    channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
    Random.shuffle(remoteExtensionBlocks).foreach(channel.writeInbound(_))
    channel.flushInbound()

    val expected = ExtensionBlocks(remoteExtensionBlocks)
    eventually {
      val actual = channel.readInbound[ExtensionBlocks]()
      actual shouldBe expected
    }
  }

  "when signatures are already in blockchain" - {
    "should propagate an empty extensions" - {
      "0" in {
        val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, mock[NgHistory]))
        channel.writeInbound(ExtensionIds(lastCommonSignature, Seq.empty))
        channel.flushInbound()

        val expected = ExtensionBlocks(Seq.empty)
        eventually {
          val actual = channel.readInbound[ExtensionBlocks]()
          actual shouldBe expected
        }
      }

      "3" in {
        val history = mock[NgHistory]
        (history.heightOf(_: ByteStr)).expects(*)
          .onCall { _: ByteStr => Some(1) }
          .repeat(remoteExtensionBlocks.size)

        val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, history))
        channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
        channel.flushInbound()

        val expected = ExtensionBlocks(Seq.empty)
        eventually {
          val actual = channel.readInbound[ExtensionBlocks]()
          actual shouldBe expected
        }
      }
    }

    "should not request blocks" in {
      val history = mock[NgHistory]
      (history.heightOf(_: ByteStr)).expects(*)
        .onCall { _: ByteStr => Some(1) }
        .repeat(remoteExtensionBlocks.size)

      val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, history))
      channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
      channel.flushInbound()

      intercept[TestFailedDueToTimeoutException] {
        eventually {
          val actual = channel.readInbound[GetBlock]()
          Option(actual) shouldBe defined
        }
      }
    }
  }

  "should ignore unexpected blocks" in {
    val remoteUnexpectedBlocks = remoteExtensionBlocks
    val remoteExtensionSignatures = remoteUnexpectedBlocks.map { _ => TestBlock.randomSignature() }

    val history = mock[NgHistory]
    (history.heightOf(_: ByteStr)).expects(*)
      .onCall { _: ByteStr => None }
      .repeat(remoteUnexpectedBlocks.size)

    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, history))
    channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
    channel.flushInbound()

    var rest = remoteUnexpectedBlocks.size
    eventually {
      channel.readOutbound[GetBlock]()
      rest -= 1
      rest shouldBe 0
    }

    remoteUnexpectedBlocks.foreach(channel.writeInbound(_))
    channel.flushInbound()

    intercept[TestFailedDueToTimeoutException] {
      eventually {
        val actual = channel.readInbound[ExtensionBlocks]()
        Option(actual) shouldBe defined
      }
    }
  }

  "blacklist a node that responses too long" in {
    val history = mock[NgHistory]
    (history.heightOf(_: ByteStr)).expects(*)
      .onCall { _: ByteStr => None }
      .repeat(remoteExtensionBlocks.size)

    var senderWasBlacklisted = false
    val peerDatabase = new PeerDatabase.NoOp {
      override def blacklistAndClose(channel: Channel, reason: String): Unit = {
        senderWasBlacklisted = true
      }
    }

    val blockSyncTimeout = 100.millis
    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(blockSyncTimeout, peerDatabase, history))
    channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
    channel.flushInbound()

    Thread.sleep(blockSyncTimeout.toMillis + 1)
    channel.runPendingTasks()
    senderWasBlacklisted shouldBe true
  }

  "should not blacklist slow node" in {
    val history = mock[NgHistory]
    (history.heightOf(_: ByteStr)).expects(*)
      .onCall { _: ByteStr => None }
      .repeat(remoteExtensionBlocks.size)

    val blockSyncTimeout = 100.millis
    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(blockSyncTimeout, PeerDatabase.NoOp, history))
    channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
    channel.flushInbound()

    Random.shuffle(remoteExtensionBlocks).foreach { block =>
      Thread.sleep((blockSyncTimeout.toMillis / 1.2).toInt)
      channel.writeInbound(block)
      channel.flushInbound()
    }

    val expected = ExtensionBlocks(remoteExtensionBlocks)
    eventually {
      val actual = channel.readInbound[ExtensionBlocks]()
      actual shouldBe expected
    }
  }

  private def createBlock(ref: ByteStr) = TestBlock.create(
    time = System.currentTimeMillis(),
    ref = ref,
    txs = Seq(transferGen.sample.get)
  )

}
