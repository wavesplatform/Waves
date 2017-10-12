package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.state2.ByteStr
import io.netty.channel.embedded.EmbeddedChannel
import monix.reactive.subjects.{AsyncSubject, ConcurrentSubject}
import org.mockito.Mockito
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.PublicKeyAccount
import scorex.block.MicroBlock
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.NgHistory
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration.DurationInt

class MicroBlockSynchronizerSpec extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with Eventually
  with GeneratorDrivenPropertyChecks
  with TransactionGen {

  private val settings = MicroblockSynchronizerSettings(
    waitResponseTimeout = 500.millis,
    processedMicroBlocksCacheTimeout = 1.second,
    invCacheTimeout = 1.second
  )

  private implicit val pc: PatienceConfig = PatienceConfig(
    timeout = 1.second,
    interval = 50.millis
  )

  "should request next block" in {
    val lastBlockSig = ByteStr("lastBlockId".getBytes)
    val nextBlockSig = ByteStr("nextBlockId".getBytes)

    val history = Mockito.mock(classOf[NgHistory])
    Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

    val channel = new EmbeddedChannel(new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, AsyncSubject[ByteStr]))
    channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig, lastBlockSig))
    channel.flushInbound()

    val r = eventually {
      val request = channel.readOutbound[MicroBlockRequest]()
      Option(request) shouldBe defined
      request
    }
    r shouldBe MicroBlockRequest(nextBlockSig)
  }

  "should not request the same block if it received before" in {
    val lastBlockSig = ByteStr("lastBlockId".getBytes)
    val nextBlockSig = ByteStr("nextBlockId".getBytes)

    val history = Mockito.mock(classOf[NgHistory])
    Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

    val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, AsyncSubject[ByteStr])
    val channel1 = new EmbeddedChannel(synchronizer)
    val channel2 = new EmbeddedChannel(synchronizer)

    channel1.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig, lastBlockSig))
    channel1.flushInbound()

    eventually {
      val request = channel1.readOutbound[MicroBlockRequest]()
      Option(request) shouldBe defined
      request.totalBlockSig shouldBe nextBlockSig
      request
    }

    channel1.writeInbound(MicroBlockResponse(MicroBlock(
      version = 1.toByte,
      generator = PublicKeyAccount("pubkey".getBytes),
      transactionData = Seq.empty,
      prevResBlockSig = lastBlockSig,
      totalResBlockSig = nextBlockSig,
      signature = nextBlockSig
    )))
    channel1.flushInbound()

    channel2.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig, lastBlockSig))
    channel2.flushInbound()

    intercept[TestFailedDueToTimeoutException] {
      eventually {
        val request = channel2.readOutbound[MicroBlockRequest]()
        Option(request) shouldBe defined
      }
    }
  }

  "should not request forked microblocks" in {
    val lastBlockSig = ByteStr("lastBlockId".getBytes)
    val nextBlockSig1 = ByteStr("nextBlockId1".getBytes)
    val nextBlockSig2 = ByteStr("nextBlockId2".getBytes)

    val history = Mockito.mock(classOf[NgHistory])
    Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

    val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, AsyncSubject[ByteStr])
    val channel = new EmbeddedChannel(synchronizer)

    channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig1, lastBlockSig))
    channel.flushInbound()

    eventually {
      val request = channel.readOutbound[MicroBlockRequest]()
      Option(request) shouldBe defined
      request.totalBlockSig shouldBe nextBlockSig1
      request
    }

    channel.writeInbound(MicroBlockResponse(MicroBlock(
      version = 1.toByte,
      generator = PublicKeyAccount("pubkey".getBytes),
      transactionData = Seq.empty,
      prevResBlockSig = lastBlockSig,
      totalResBlockSig = nextBlockSig1,
      signature = nextBlockSig1
    )))
    channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig2, lastBlockSig))
    channel.flushInbound()

    intercept[TestFailedDueToTimeoutException] {
      eventually {
        val request = channel.readOutbound[MicroBlockRequest]()
        Option(request) shouldBe defined
      }
    }
  }

  "should remember MicroblockInv to make a request further" in {
    val lastBlockSig = ByteStr("lastBlockId".getBytes)
    val nextBlockSig1 = ByteStr("nextBlockId1".getBytes)
    val nextBlockSig2 = ByteStr("nextBlockId2".getBytes)

    val history = Mockito.mock(classOf[NgHistory])
    Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

    val events = ConcurrentSubject.publish[ByteStr]
    val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, events)
    val channel = new EmbeddedChannel(synchronizer)

    channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig1, lastBlockSig))
    channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig2, nextBlockSig1))
    channel.flushInbound()

    eventually {
      val request = channel.readOutbound[MicroBlockRequest]()
      Option(request) shouldBe defined
      request
    }

    channel.writeInbound(MicroBlockResponse(MicroBlock(
      version = 1.toByte,
      generator = PublicKeyAccount("pubkey".getBytes),
      transactionData = Seq.empty,
      prevResBlockSig = lastBlockSig,
      totalResBlockSig = nextBlockSig1,
      signature = nextBlockSig1
    )))
    channel.flushInbound()
    events.onNext(nextBlockSig1)

    eventually {
      val request = channel.readOutbound[MicroBlockRequest]()
      Option(request) shouldBe defined
      request.totalBlockSig shouldBe nextBlockSig2
      request
    }
  }

}
