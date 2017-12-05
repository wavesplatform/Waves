package com.wavesplatform.network

import java.nio.charset.StandardCharsets

import com.wavesplatform.TransactionGen
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.embedded.EmbeddedChannel
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.{SignedTransaction, Transaction}

class MessageCodecSpec extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with TransactionGen {

  "should block a sender of invalid messages" in {
    val codec = new SpiedMessageCodec
    val ch = new EmbeddedChannel(codec)

    ch.writeInbound(RawBytes(TransactionMessageSpec.messageCode, "foo".getBytes(StandardCharsets.UTF_8)))
    ch.readInbound[IssueTransaction]()

    codec.blockCalls shouldBe 1
  }

  "should not block a sender of valid messages" in forAll(randomTransactionGen) { origTx : SignedTransaction =>
    val codec = new SpiedMessageCodec
    val ch = new EmbeddedChannel(codec)

    ch.writeInbound(RawBytes(TransactionMessageSpec.messageCode, origTx.bytes()))
    val decodedTx = ch.readInbound[Transaction]()

    decodedTx shouldBe origTx
    codec.blockCalls shouldBe 0
  }

  private class SpiedMessageCodec extends MessageCodec(PeerDatabase.NoOp) {
    var blockCalls = 0

    override def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
      blockCalls += 1
    }
  }

}
