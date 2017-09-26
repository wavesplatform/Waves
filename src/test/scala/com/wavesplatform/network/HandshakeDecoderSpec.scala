package com.wavesplatform.network

import java.nio.charset.StandardCharsets

import com.wavesplatform.TransactionGen
import io.netty.buffer.Unpooled
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpec, Matchers}

class HandshakeDecoderSpec extends FreeSpec
  with Matchers
  with MockFactory
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with TransactionGen {

  "should read a handshake and remove itself from the pipeline" in {
    var mayBeDecodedHandshake: Option[Handshake] = None

    val channel = new EmbeddedChannel(
      new HandshakeDecoder(),
      new ChannelInboundHandlerAdapter {
        override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = msg match {
          case x: Handshake => mayBeDecodedHandshake = Some(x)
          case _ =>
        }
      }
    )

    val origHandshake = new Handshake(
      applicationName = "wavesI",
      applicationVersion = (1, 2, 3),
      nodeName = "test",
      nodeNonce = 4,
      declaredAddress = None
    )

    val buff = Unpooled.buffer
    origHandshake.encode(buff)
    buff.writeCharSequence("foo", StandardCharsets.UTF_8)

    channel.writeInbound(buff)

    mayBeDecodedHandshake should contain(origHandshake)
  }

}
