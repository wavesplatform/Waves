package com.wavesplatform.network

import java.nio.charset.StandardCharsets

import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.{IssueTransaction, UpdateAssetInfoTransaction}
import com.wavesplatform.transaction.{ProvenTransaction, Transaction}
import com.wavesplatform.{TestValues, TransactionGen}
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.embedded.EmbeddedChannel
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class MessageCodecSpec extends FreeSpec with Matchers with MockFactory with PropertyChecks with TransactionGen {

  "should block a sender of invalid messages" in {
    val codec = new SpiedMessageCodec
    val ch    = new EmbeddedChannel(codec)

    ch.writeInbound(RawBytes(TransactionSpec.messageCode, "foo".getBytes(StandardCharsets.UTF_8)))
    ch.readInbound[IssueTransaction]()

    codec.blockCalls shouldBe 1
  }

  "should not block a sender of valid messages" in forAll(randomTransactionGen) { origTx: ProvenTransaction =>
    val codec = new SpiedMessageCodec
    val ch    = new EmbeddedChannel(codec)

    ch.writeInbound(RawBytes.fromTransaction(origTx))
    val decodedTx = ch.readInbound[Transaction]()

    decodedTx shouldBe origTx
    codec.blockCalls shouldBe 0
  }

  "should pack update asset info in PB message" in {
    val tx = UpdateAssetInfoTransaction
      .selfSigned(1, TestValues.keyPair, TestValues.asset.id, "bomz", "", System.currentTimeMillis(), TestValues.fee, Waves)
      .right
      .get
    RawBytes.fromTransaction(tx) shouldBe RawBytes(PBTransactionSpec.messageCode, PBTransactionSpec.serializeData(tx))
  }

  private class SpiedMessageCodec extends MessageCodec(PeerDatabase.NoOp) {
    var blockCalls = 0

    override def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
      blockCalls += 1
    }
  }

}
