package com.wavesplatform.network

import java.nio.charset.StandardCharsets

import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{ProvenTransaction, Transaction}
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.embedded.EmbeddedChannel

class MessageCodecSpec extends FreeSpec {

  "should block a sender of invalid messages" in {
    val codec = new SpyingMessageCodec
    val ch    = new EmbeddedChannel(codec)

    ch.writeInbound(RawBytes(TransactionSpec.messageCode, "foo".getBytes(StandardCharsets.UTF_8)))
    ch.readInbound[IssueTransaction]()

    codec.blockCalls shouldBe 1
  }

  "should not block a sender of valid messages" in forAll(randomTransactionGen) { (origTx: Transaction with ProvenTransaction) =>
    val codec = new SpyingMessageCodec
    val ch    = new EmbeddedChannel(codec)

    ch.writeInbound(RawBytes.fromTransaction(origTx))
    val decodedTx = ch.readInbound[Transaction]()

    decodedTx shouldBe origTx
    codec.blockCalls shouldBe 0
  }

  private class SpyingMessageCodec extends MessageCodec(PeerDatabase.NoOp) {
    var blockCalls = 0

    override def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
      blockCalls += 1
    }
  }

}
