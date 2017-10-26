package com.wavesplatform.network

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.embedded.EmbeddedChannel
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration.DurationInt

class ExtensionSignaturesLoaderSpec extends FreeSpec
  with Matchers
  with MockFactory
  with Eventually
  with TransactionGen {

  private val lastCommonSignature = ByteStr("1".getBytes)
  private val localSignatures = Seq(lastCommonSignature, ByteStr("2".getBytes))
  private val remoteSignatures = Seq(lastCommonSignature, ByteStr("3".getBytes))

  "should request signatures on an extension request" in {
    val channel = new EmbeddedChannel(new ExtensionSignaturesLoader(1.minute, PeerDatabase.NoOp))
    channel.writeOutbound(LoadBlockchainExtension(localSignatures))
    channel.flushOutbound()

    val expected = GetSignatures(localSignatures)
    eventually {
      val actual = channel.readOutbound[GetSignatures]()
      actual shouldBe expected
    }
  }

  "should send extension's ids down to the channel when signatures are came" in {
    val channel = new EmbeddedChannel(new ExtensionSignaturesLoader(1.minute, PeerDatabase.NoOp))
    channel.writeOutbound(LoadBlockchainExtension(localSignatures))
    channel.flushOutbound()

    eventually {
      val actual = channel.readOutbound[GetSignatures]()
      Option(actual) shouldBe defined
    }

    channel.writeInbound(Signatures(remoteSignatures))
    channel.flushInbound()

    val expected = ExtensionIds(lastCommonSignature, remoteSignatures.tail)
    eventually {
      val actual = channel.readInbound[ExtensionIds]()
      actual shouldBe expected
    }
  }

  "should timeout long requests" in {
    var isTimedOut = false
    val channel = new EmbeddedChannel(new ExtensionSignaturesLoader(100.millis, PeerDatabase.NoOp) {
      override protected def onResponseTimedOut(ctx: ChannelHandlerContext): Unit = {
        isTimedOut = true
      }
    })
    channel.writeOutbound(LoadBlockchainExtension(localSignatures))
    channel.flushOutbound()

    intercept[TestFailedDueToTimeoutException] {
      eventually {
        isTimedOut shouldBe true
      }
    }
  }

}
