package com.wavesplatform.discovery.actors

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.TimeUnit

import akka.actor.Actor
import com.wavesplatform.discovery._
import com.wavesplatform.discovery.network._
import com.wavesplatform.network.{GetPeers, Handshake, KnownPeers, LegacyFrameCodec, PeerDatabase, PipelineInitializer}
import io.netty.bootstrap.Bootstrap
import io.netty.channel.Channel
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

object PeerDiscoveryActor {

  case class GetPeersFrom(peer: InetSocketAddress)

  val peerDatabaseStub = new PeerDatabase {
    override def suspend(host: InetAddress): Unit = {}

    override def knownPeers: Map[InetSocketAddress, Long] = Map.empty

    override def randomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress] = None

    override def blacklist(host: InetAddress, reason: String): Unit = {}

    override def touch(socketAddress: InetSocketAddress): Unit = {}

    override def suspendedHosts: Set[InetAddress] = Set.empty

    override def blacklistedHosts: Set[InetAddress] = Set.empty

    override def detailedBlacklist: Map[InetAddress, (Long, String)] = Map.empty

    override def clearBlacklist(): Unit = {}

    override def detailedSuspended: Map[InetAddress, Long] = Map.empty

    override def addCandidate(socketAddress: InetSocketAddress): Unit = {}

    override def blacklistAndClose(channel: Channel, reason: String): Unit = {}
  }
}

class PeerDiscoveryActor(chainId: Char) extends Actor {

  import PeerDiscoveryActor._

  def receive: PartialFunction[Any, Unit] = {
    case GetPeersFrom(peer) => context.parent ! MainActor.PeerInfo(peer, getPeersFromNode(peer))
  }

  private val getPeersTimeout = 10

  private def getPeersFromNode(address: InetSocketAddress): Set[InetSocketAddress] = {
    var peers: Set[InetSocketAddress] = Set.empty

    val exceptionHandler = new ExceptionHandler()

    implicit val workerGroup: NioEventLoopGroup = new NioEventLoopGroup

    new Bootstrap()
      .group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .handler(new PipelineInitializer[SocketChannel](Seq(
        exceptionHandler,
        new HandshakeHandler(chainId),
        new LengthFieldPrepender(4),
        new LengthFieldBasedFrameDecoder(100 * 1024 * 1024, 0, 4, 0, 4),
        new LegacyFrameCodec(peerDatabaseStub),
        new MessageCodec(),
        new MessageHandler({
          case (msg, ctx) =>
            msg match {
              case hs: Handshake => ctx.writeAndFlush(GetPeers)
              case KnownPeers(p) => peers = p.toSet; ctx.close()
              case _             =>
            }
        })
      )))
      .remoteAddress(address.getAddress, address.getPort)
      .connect()

    Await.result(exceptionHandler.closed, new FiniteDuration(getPeersTimeout, TimeUnit.SECONDS))
    workerGroup.shutdownGracefully()
    peers
  }

}
