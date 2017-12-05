package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.settings.SynchronizationSettings.MicroblockSynchronizerSettings
import com.wavesplatform.state2.ByteStr
import io.netty.channel.embedded.EmbeddedChannel
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.ConcurrentSubject
import org.mockito.Mockito
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.PublicKeyAccount
import scorex.block.MicroBlock
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.NgHistory

import scala.concurrent.duration.DurationInt

class MicroBlockSynchronizerSpec extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with Eventually
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

  private val generator = PublicKeyAccount("generator".getBytes())
  private val responseSig = ByteStr("signature".getBytes())
  private def microBlockOwners = new MicroBlockOwners(10.minutes)

  "should request next block" in {
    val lastBlockSig = ByteStr("0".getBytes)
    val nextBlockSig = ByteStr("1".getBytes)

    val history = Mockito.mock(classOf[NgHistory])
    Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

    val channel = new EmbeddedChannel(new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, ConcurrentSubject.publish[ByteStr], microBlockOwners))
    channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig, lastBlockSig))
    channel.flushInbound()

    val r = eventually {
      val request = channel.readOutbound[MicroBlockRequest]()
      Option(request) shouldBe defined
      request
    }
    r shouldBe MicroBlockRequest(nextBlockSig)
  }

  "should re-request next block if a previous one was failed" in {
    val lastBlockSig = ByteStr("0".getBytes)
    val nextBlockSig = ByteStr("1".getBytes)

    val history = Mockito.mock(classOf[NgHistory])
    Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

    val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, ConcurrentSubject.publish[ByteStr], microBlockOwners)
    val channel1 = new EmbeddedChannel(synchronizer)
    val channel2 = new EmbeddedChannel(synchronizer)

    val inv = MicroBlockInv(TestBlock.defaultSigner, nextBlockSig, lastBlockSig)
    channel1.writeInbound(inv)
    channel1.flushInbound()
    channel1.close()

    channel2.writeInbound(inv)
    channel2.flushInbound()

    val r = eventually {
      val request = channel2.readOutbound[MicroBlockRequest]()
      Option(request) shouldBe defined
      request
    }
    r shouldBe MicroBlockRequest(nextBlockSig)
  }

  "should not request the same block if it received before" - {
    "from same channel" in {
      val lastBlockSig = ByteStr("0".getBytes)
      val nextBlockSig = ByteStr("1".getBytes)

      val history = Mockito.mock(classOf[NgHistory])
      Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

      val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, ConcurrentSubject.publish[ByteStr], microBlockOwners)
      val channel = new EmbeddedChannel(synchronizer)

      channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig, lastBlockSig))
      channel.flushInbound()

      eventually {
        val request = channel.readOutbound[MicroBlockRequest]()
        Option(request) shouldBe defined
        request.totalBlockSig shouldBe nextBlockSig
        request
      }

      channel.writeInbound(MicroBlockResponse(MicroBlock(
        version = 1.toByte,
        generator = generator,
        transactionData = Seq.empty,
        prevResBlockSig = lastBlockSig,
        totalResBlockSig = nextBlockSig,
        signature = responseSig
      )))
      channel.flushInbound()

      channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig, lastBlockSig))
      channel.flushInbound()

      intercept[TestFailedDueToTimeoutException] {
        eventually {
          val request = channel.readOutbound[MicroBlockRequest]()
          Option(request) shouldBe defined
        }
      }
    }

    "from another channel" in {
      val lastBlockSig = ByteStr("0".getBytes)
      val nextBlockSig = ByteStr("1".getBytes)

      val history = Mockito.mock(classOf[NgHistory])
      Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

      val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, ConcurrentSubject.publish[ByteStr], microBlockOwners)
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
        generator = generator,
        transactionData = Seq.empty,
        prevResBlockSig = lastBlockSig,
        totalResBlockSig = nextBlockSig,
        signature = responseSig
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
  }

  "should not request forked microblocks" in {
    val lastBlockSig = ByteStr("0".getBytes)
    val nextBlockSig1 = ByteStr("1".getBytes)
    val nextBlockSig2 = ByteStr("1'".getBytes)

    val history = Mockito.mock(classOf[NgHistory])
    Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

    val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, ConcurrentSubject.publish[ByteStr], microBlockOwners)
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
      generator = generator,
      transactionData = Seq.empty,
      prevResBlockSig = lastBlockSig,
      totalResBlockSig = nextBlockSig1,
      signature = responseSig
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
    val lastBlockSig = ByteStr("0".getBytes)
    val nextBlockSig1 = ByteStr("1".getBytes)
    val nextBlockSig2 = ByteStr("2".getBytes)

    val history = Mockito.mock(classOf[NgHistory])
    Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

    val events = ConcurrentSubject.publish[ByteStr]
    val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, events, microBlockOwners)
    val channel = new EmbeddedChannel(synchronizer)

    channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig1, lastBlockSig))
    channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, nextBlockSig2, nextBlockSig1))
    channel.flushInbound()

    eventually {
      val request = channel.readOutbound[MicroBlockRequest]()
      Option(request) shouldBe defined
    }

    channel.writeInbound(MicroBlockResponse(MicroBlock(
      version = 1.toByte,
      generator = generator,
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

  "should request multiple microblocks from the chain of invs" - {
    val sigPairs = (0 to 9).sliding(2).map {
      case Seq(prev, next) => (ByteStr(s"$prev".getBytes), ByteStr(s"$next".getBytes))
    }

    "multiple at once" in {
      val lastBlockSig = ByteStr("0".getBytes)

      val history = Mockito.mock(classOf[NgHistory])
      Mockito.doReturn(Some(lastBlockSig)).when(history).lastBlockId()

      val events = ConcurrentSubject.publish[ByteStr]
      val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, events, microBlockOwners)
      val channel = new EmbeddedChannel(synchronizer)

      sigPairs.foreach {
        case (prev, next) => channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, next, prev))
      }
      channel.flushInbound()

      sigPairs.foreach {
        case (prev, next) =>
          eventually {
            val request = channel.readOutbound[MicroBlockRequest]()
            Option(request) shouldBe defined
            request.totalBlockSig shouldBe next
          }

          channel.writeInbound(MicroBlockResponse(MicroBlock(
            version = 1.toByte,
            generator = generator,
            transactionData = Seq.empty,
            prevResBlockSig = prev,
            totalResBlockSig = next,
            signature = responseSig
          )))

          channel.flushInbound()
          events.onNext(next)
      }
    }

    "in sequence" in {
      val history = Mockito.mock(classOf[NgHistory])
      val events = ConcurrentSubject.publish[ByteStr]
      val synchronizer = new MicroBlockSynchronizer(settings, history, PeerDatabase.NoOp, events, microBlockOwners)
      val channel = new EmbeddedChannel(synchronizer)

      sigPairs.foreach {
        case (prev, next) =>
          Mockito.doReturn(Some(prev)).when(history).lastBlockId()

          channel.writeInbound(MicroBlockInv(TestBlock.defaultSigner, next, prev))
          channel.flushInbound()

          eventually {
            val request = channel.readOutbound[MicroBlockRequest]()
            Option(request) shouldBe defined
            request.totalBlockSig shouldBe next
          }

          channel.writeInbound(MicroBlockResponse(MicroBlock(
            version = 1.toByte,
            generator = generator,
            transactionData = Seq.empty,
            prevResBlockSig = prev,
            totalResBlockSig = next,
            signature = responseSig
          )))
          channel.flushInbound()
      }
    }
  }

}
