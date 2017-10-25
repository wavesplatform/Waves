package com.wavesplatform

import java.net.InetSocketAddress

import com.wavesplatform.discovery.network.{HandshakeHandler, LegacyFrameCodec, MessageCodec, MessageHandler}
import com.wavesplatform.network.{GetPeers, Handshake, KnownPeers, PipelineInitializer}
import io.netty.bootstrap.Bootstrap
import io.netty.channel.EventLoopGroup
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}

import scala.concurrent.{ExecutionContext, Future}

package object discovery {

  implicit val workerGroup = new NioEventLoopGroup
  implicit val ec: ExecutionContext = ExecutionContext.global

  def getPeersFromNode(address: InetSocketAddress)(implicit eventLoopGroup: EventLoopGroup): scala.concurrent.Future[Set[InetSocketAddress]] = Future {
    var peers: Set[InetSocketAddress] = Set.empty

    new Bootstrap()
      .group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .handler(new PipelineInitializer[SocketChannel](Seq(
        new HandshakeHandler(),
        new LengthFieldPrepender(4),
        new LengthFieldBasedFrameDecoder(100 * 1024 * 1024, 0, 4, 0, 4),
        new LegacyFrameCodec(),
        new MessageCodec(),
        new MessageHandler({ case (msg, ctx) =>
          msg match {
            case hs: Handshake => ctx.writeAndFlush(GetPeers)
            case KnownPeers(p) => peers = p.toSet; ctx.close()
            case _ =>
          }
        })
      )))
      .remoteAddress(address.getAddress, address.getPort)
      .connect().channel().closeFuture().sync()

    peers
  }
}
