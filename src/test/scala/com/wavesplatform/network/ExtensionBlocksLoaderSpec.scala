package com.wavesplatform.network

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.ByteStr
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.{FreeSpec, Matchers}
import scorex.block.{Block, SignerData}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.NgHistory
import scorex.utils.Synchronized
import scorex.utils.Synchronized.ReadLock

import scala.concurrent.duration.DurationInt
import scala.util.Random

class ExtensionBlocksLoaderSpec extends FreeSpec
  with Matchers
  with MockFactory
  with Eventually
  with TransactionGen {

  private val lastCommonBlock = TestBlock.create(transferGen.sample.take(1).toSeq)
  private val lastCommonSignature: ByteStr = lastCommonBlock.uniqueId
  private val remoteExtensionBlocks: Seq[Block] = (1 to 3)
    .scanLeft(lastCommonBlock) { (r, _) => createBlock(r.uniqueId) }
    .tail // without a common block

  private val remoteExtensionSignatures: Seq[ByteStr] = remoteExtensionBlocks.map(_.uniqueId)

  "should request blocks" in {
    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, newHistory()))
    channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
    channel.flushInbound()

    var rest = remoteExtensionSignatures.map(GetBlock).toSet
    eventually {
      val actual = channel.readOutbound[GetBlock]()
      Option(actual).foreach { x =>
        if (rest.contains(x)) rest -= x
        else fail(s"Unexpected block $x, expected: $rest")
      }
      rest shouldBe empty
    }
  }

  "should propagate an extension after receiving all blocks" in {
    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, newHistory()))
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

  "should ignore" - {
    "unexpected blocks" in {
      val remoteUnexpectedBlocks = remoteExtensionBlocks
      val remoteExtensionSignatures = remoteUnexpectedBlocks.map { _ => TestBlock.randomSignature() }

      val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, newHistory()))
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

    "new extension ids if the previous are downloading" in {
      val remoteUnexpectedBlocks = remoteExtensionBlocks
      val remoteUnexpectedExtensionSignatures = remoteUnexpectedBlocks.map { _ => TestBlock.randomSignature() }

      val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, newHistory()))
      channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
      channel.writeInbound(ExtensionIds(lastCommonSignature, remoteUnexpectedExtensionSignatures))
      channel.flushInbound()

      val unexpected = remoteUnexpectedExtensionSignatures.toSet
      eventually {
        val blockRequest = channel.readOutbound[GetBlock]()
        Option(blockRequest).foreach { x =>
          unexpected shouldNot contain(x.signature)
        }
      }
    }

    "an extension if it has lower score than the local one" in {
      val history = newHistory(TestBlock.sign(
        signer = TestBlock.defaultSigner,
        b = Block(
          timestamp = System.currentTimeMillis(),
          version = 2,
          reference = TestBlock.randomSignature(),
          signerData = SignerData(TestBlock.defaultSigner, TestBlock.randomSignature()),
          consensusData = NxtLikeConsensusBlockData(1L, ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte))),
          transactionData = transferGen.sample.take(3).toList,
          featureVotes = Set.empty
        )
      ))

      val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, PeerDatabase.NoOp, history))
      channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
      remoteExtensionBlocks.foreach(channel.writeInbound(_))
      channel.flushInbound()

      intercept[TestFailedDueToTimeoutException] {
        eventually {
          val actual = channel.readInbound[ExtensionBlocks]()
          Option(actual) shouldBe defined
        }
      }
    }
  }

  "blacklist a node" - {
    "sends a chain does not reference to the common block" in chainTest(Seq(createBlock(TestBlock.randomSignature())))

    "sends an invalid chain" in chainTest(
      createBlock(lastCommonSignature) +: (1 to 3).map(_ => createBlock(TestBlock.randomSignature()))
    )

    "sends an invalid blocks" in chainTest(
      (1 to 3)
        .scanLeft(lastCommonBlock) { (r, _) =>
          Block(
            timestamp = System.currentTimeMillis(),
            version = 2,
            reference = r.uniqueId,
            signerData = SignerData(TestBlock.defaultSigner, TestBlock.randomSignature()),
            consensusData = NxtLikeConsensusBlockData(1L, ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte))),
            transactionData = transferGen.sample.take(3).toList,
            featureVotes = Set.empty
          )
        }
        .tail
    )

    "responses too long" in {
      var senderWasBlacklisted = false
      val peerDatabase = new PeerDatabase.NoOp {
        override def blacklistAndClose(channel: Channel, reason: String): Unit = {
          senderWasBlacklisted = true
        }
      }

      val blockSyncTimeout = 100.millis
      val channel = new EmbeddedChannel(new ExtensionBlocksLoader(blockSyncTimeout, peerDatabase, newHistory()))
      channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
      channel.flushInbound()

      Thread.sleep(blockSyncTimeout.toMillis + 1)
      channel.runPendingTasks()
      senderWasBlacklisted shouldBe true
    }

    def chainTest(remoteExtensionBlocks: Seq[Block]): Unit = {
      val remoteExtensionSignatures = remoteExtensionBlocks.map(_.uniqueId)

      var senderWasBlacklisted = false
      val peerDatabase = new PeerDatabase.NoOp {
        override def blacklistAndClose(channel: Channel, reason: String): Unit = {
          senderWasBlacklisted = true
        }
      }

      val channel = new EmbeddedChannel(new ExtensionBlocksLoader(1.minute, peerDatabase, newHistory()))
      channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
      remoteExtensionBlocks.foreach(channel.writeInbound(_))
      channel.flushInbound()

      eventually {
        senderWasBlacklisted shouldBe true
      }
    }
  }

  "should not blacklist slow (but acceptable) node" in {
    val blockSyncTimeout = 100.millis
    val channel = new EmbeddedChannel(new ExtensionBlocksLoader(blockSyncTimeout, PeerDatabase.NoOp, newHistory()))
    channel.writeInbound(ExtensionIds(lastCommonSignature, remoteExtensionSignatures))
    channel.flushInbound()

    Random.shuffle(remoteExtensionBlocks).foreach { block =>
      Thread.sleep((blockSyncTimeout.toMillis / 1.2).toInt)
      channel.writeInbound(block)
      channel.flushInbound()
      channel.runPendingTasks()
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
    txs = transferGen.sample.take(3).toList
  )

  private def newHistory(): NgHistory = newHistory(TestBlock.sign(
    signer = TestBlock.defaultSigner,
    b = Block(
      timestamp = System.currentTimeMillis(),
      version = 2,
      reference = TestBlock.randomSignature(),
      signerData = SignerData(TestBlock.defaultSigner, TestBlock.randomSignature()),
      consensusData = NxtLikeConsensusBlockData(3L, ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte))),
      transactionData = transferGen.sample.take(3).toList,
      featureVotes = Set.empty
    )
  ))

  private def newHistory(lastHistoryBlock: Block): NgHistory = {
    val lastHistoryBlockHeight = 10
    val history = mock[NgHistory]

    (history.height _).expects().returning(lastHistoryBlockHeight).anyNumberOfTimes()

    (history.blockAt _).expects(*)
      .onCall { (x: Int) =>
        if (x == lastHistoryBlockHeight) Some(lastHistoryBlock) else None
      }
      .anyNumberOfTimes()

    (history.heightOf(_: ByteStr)).expects(*)
      .onCall { (_: ByteStr) => None }
      .anyNumberOfTimes()

    (history.scoreOf _).expects(*)
      .onCall { (x: ByteStr) =>
        if (x == lastHistoryBlock.uniqueId) Some(lastHistoryBlock.blockScore)
        else None
      }
      .anyNumberOfTimes()

    (history.read(_: ReadLock => Any)).expects(*)
      .onCall { (f: (ReadLock => Any)) => f(new Synchronized.ReadLock(new ReentrantReadWriteLock())) }
      .anyNumberOfTimes()

    history
  }

}
