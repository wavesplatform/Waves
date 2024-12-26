package com.wavesplatform.network.client

import java.io.IOException

import com.wavesplatform.network.*
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.*
import io.netty.channel.socket.SocketChannel
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}

import scala.concurrent.Promise
import scala.concurrent.duration.*

class ClientHandshakeHandler(handshake: Handshake, promise: Promise[Channel]) extends ChannelInboundHandlerAdapter with ScorexLogging {

  private def removeHandlers(ctx: ChannelHandlerContext): Unit = {
    ctx.pipeline().remove(classOf[HandshakeTimeoutHandler])
    ctx.pipeline().remove(this)
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case HandshakeTimeoutExpired =>
      log.error("Timeout expired while waiting for handshake")
      ctx.close()
      promise.failure(new IOException("No handshake"))
    case remoteHandshake: Handshake =>
      if (handshake.applicationName != remoteHandshake.applicationName) {
        log.warn(s"Remote application name ${remoteHandshake.applicationName} does not match local ${handshake.applicationName}")
        ctx.close()
      } else {
        promise.success(ctx.channel())
        log.info(s"Accepted handshake $remoteHandshake")
        removeHandlers(ctx)
      }
    case _ => super.channelRead(ctx, msg)
  }

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    ctx.writeAndFlush(handshake.encode(ctx.alloc().buffer()))
    super.channelActive(ctx)
  }
}

// Used only in tests and Generator
class LegacyChannelInitializer(trafficLoggerSettings: TrafficLogger.Settings, handshake: Handshake, promise: Promise[Channel])
  extends ChannelInitializer[SocketChannel] {
  private val lengthFieldLength = 4
  private val maxFieldLength    = 1024 * 1024

  override def initChannel(ch: SocketChannel): Unit =
    ch.pipeline()
      .addLast(
        new HandshakeDecoder(PeerDatabase.NoOp),
        new HandshakeTimeoutHandler(30.seconds),
        new ClientHandshakeHandler(handshake, promise),
        new LengthFieldPrepender(lengthFieldLength),
        new LengthFieldBasedFrameDecoder(maxFieldLength, 0, lengthFieldLength, 0, lengthFieldLength),
        new LegacyFrameCodecL1(PeerDatabase.NoOp, 3.minutes),
        new TrafficLoggerL1(trafficLoggerSettings)
      )
}
