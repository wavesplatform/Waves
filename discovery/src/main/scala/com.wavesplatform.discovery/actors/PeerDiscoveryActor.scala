package com.wavesplatform.discovery.actors

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import akka.actor.Actor
import com.wavesplatform.discovery._
import com.wavesplatform.discovery.network._
import com.wavesplatform.network.{GetPeers, Handshake, KnownPeers, PipelineInitializer}
import io.netty.bootstrap.Bootstrap
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

object PeerDiscoveryActor {
  case class GetPeersFrom(peer: InetSocketAddress)
}

class PeerDiscoveryActor(chainId: Char) extends Actor {

  import PeerDiscoveryActor._

  def receive: PartialFunction[Any, Unit] = {
    case GetPeersFrom(peer) => context.parent ! MainActor.PeerInfo(peer, getPeersFromNode(peer))
  }

  private def getPeersFromNode(address: InetSocketAddress): Set[InetSocketAddress]= {
    var peers: Set[InetSocketAddress] = Set.empty

    val exceptionHandler = new ExceptionHandler()

    new Bootstrap()
      .group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .handler(new PipelineInitializer[SocketChannel](Seq(
        exceptionHandler,
        new HandshakeHandler(chainId),
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
      .connect()

    Await.result(exceptionHandler.closed, new FiniteDuration(10, TimeUnit.SECONDS))

    peers
  }

}