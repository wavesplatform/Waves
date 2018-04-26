package com.wavesplatform.network

import com.wavesplatform.{OldTransactionGen, crypto}
import io.netty.buffer.Unpooled.wrappedBuffer
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.embedded.EmbeddedChannel
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import scorex.network.message.{Message => ScorexMessage}
import scorex.transaction.Transaction

class LegacyFrameCodecSpec extends FreeSpec with Matchers with MockFactory with PropertyChecks with OldTransactionGen {

  "should handle one message" in forAll(issueGen) { origTx =>
    val codec = new LegacyFrameCodec(PeerDatabase.NoOp)

    val buff = Unpooled.buffer
    write(buff, origTx)

    val ch = new EmbeddedChannel(codec)
    ch.writeInbound(buff)

    val decodedBytes = ch.readInbound[RawBytes]()

    decodedBytes.code shouldBe TransactionSpec.messageCode
    decodedBytes.data shouldEqual origTx.bytes()
  }

  "should handle multiple messages" in forAll(Gen.nonEmptyListOf(issueGen)) { origTxs =>
    val codec = new LegacyFrameCodec(PeerDatabase.NoOp)

    val buff = Unpooled.buffer
    origTxs.foreach(write(buff, _))

    val ch = new EmbeddedChannel(codec)
    ch.writeInbound(buff)

    val decoded = (1 to origTxs.size).map { _ =>
      ch.readInbound[RawBytes]()
    }

    val decodedTxs = decoded.map { x =>
      TransactionSpec.deserializeData(x.data).get
    }

    decodedTxs shouldEqual origTxs
  }

  private def write(buff: ByteBuf, tx: Transaction): Unit = {
    val txBytes  = tx.bytes()
    val checkSum = wrappedBuffer(crypto.fastHash(txBytes), 0, ScorexMessage.ChecksumLength)

    buff.writeInt(LegacyFrameCodec.Magic)
    buff.writeByte(TransactionSpec.messageCode)
    buff.writeInt(txBytes.length)
    buff.writeBytes(checkSum)
    buff.writeBytes(txBytes)
  }

}
