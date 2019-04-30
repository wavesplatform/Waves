package com.wavesplatform.network

import java.net.InetSocketAddress

import com.wavesplatform.network.message.{MessageSpec, Message => ScorexMessage}
import com.wavesplatform.{TransactionGen, crypto}
import io.netty.buffer.Unpooled.wrappedBuffer
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.embedded.EmbeddedChannel
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.concurrent.duration.DurationInt

class LegacyFrameCodecSpec extends FreeSpec with Matchers with MockFactory with PropertyChecks with TransactionGen {

  "should handle one message" in forAll(issueGen) { origTx =>
    val codec = new LegacyFrameCodec(PeerDatabase.NoOp, 3.minutes)

    val buff = Unpooled.buffer
    write(buff, origTx, TransactionSpec)

    val ch = new EmbeddedChannel(codec)
    ch.writeInbound(buff)

    val decodedBytes = ch.readInbound[RawBytes]()

    decodedBytes.code shouldBe TransactionSpec.messageCode
    decodedBytes.data shouldEqual origTx.bytes()
  }

  "should handle multiple messages" in forAll(Gen.nonEmptyListOf(issueGen)) { origTxs =>
    val codec = new LegacyFrameCodec(PeerDatabase.NoOp, 3.minutes)

    val buff = Unpooled.buffer
    origTxs.foreach(write(buff, _, TransactionSpec))

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

  "should reject an already received transaction" in {
    val tx    = issueGen.sample.getOrElse(throw new RuntimeException("Can't generate a sample transaction"))
    val codec = new LegacyFrameCodec(PeerDatabase.NoOp, 3.minutes)
    val ch    = new EmbeddedChannel(codec)

    val buff1 = Unpooled.buffer
    write(buff1, tx, TransactionSpec)
    ch.writeInbound(buff1)

    val buff2 = Unpooled.buffer
    write(buff2, tx, TransactionSpec)
    ch.writeInbound(buff2)

    ch.inboundMessages().size() shouldEqual 1
  }

  "should not reject an already received GetPeers" in {
    val msg   = KnownPeers(Seq(InetSocketAddress.createUnresolved("127.0.0.1", 80)))
    val codec = new LegacyFrameCodec(PeerDatabase.NoOp, 3.minutes)
    val ch    = new EmbeddedChannel(codec)

    val buff1 = Unpooled.buffer
    write(buff1, msg, PeersSpec)
    ch.writeInbound(buff1)

    val buff2 = Unpooled.buffer
    write(buff2, msg, PeersSpec)
    ch.writeInbound(buff2)

    ch.inboundMessages().size() shouldEqual 2
  }

  private def write[T <: AnyRef](buff: ByteBuf, msg: T, spec: MessageSpec[T]): Unit = {
    val bytes    = spec.serializeData(msg)
    val checkSum = wrappedBuffer(crypto.fastHash(bytes), 0, ScorexMessage.ChecksumLength)

    buff.writeInt(LegacyFrameCodec.Magic)
    buff.writeByte(spec.messageCode)
    buff.writeInt(bytes.length)
    buff.writeBytes(checkSum)
    buff.writeBytes(bytes)
  }

}
