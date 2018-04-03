package com.wavesplatform.discovery.actors

import java.net.InetSocketAddress

import akka.actor.SupervisorStrategy.Resume
import akka.actor.{Actor, ActorRef, ActorSystem, OneForOneStrategy, Props, SupervisorStrategy}
import akka.routing.{ActorRefRoutee, Router}
import com.wavesplatform.discovery.collections.{ExpirationSet, Pool}
import com.wavesplatform.discovery.routers.SmallestMailboxWithThresholdRoutingLogic
import play.api.libs.json._

class MainActor(chainId: Char, workersCount: Int) extends Actor {

  import MainActor._

  private val mailboxThreshold = 5
  private val router = {
    val routes = Vector.fill(workersCount) {
      ActorRefRoutee(context.actorOf(Props(classOf[PeerDiscoveryActor], chainId)))
    }
    Router(SmallestMailboxWithThresholdRoutingLogic(mailboxThreshold), routes)
  }

  private val alivePeers            = new Pool[InetSocketAddress]
  private val deadPeersCacheTimeout = 5
  private val deadPeers             = new ExpirationSet[InetSocketAddress](1000 * 60 * 60 * 1)
  private val peerResponses         = scala.collection.mutable.Map.empty[InetSocketAddress, Set[InetSocketAddress]]
  private val connections           = scala.collection.mutable.Set.empty[ActorRef]

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
    case _: Exception => Resume
  }

  def receive: PartialFunction[Any, Unit] = {

    case Peers(p) => (p -- deadPeers).foreach(alivePeers.add)

    case Discover => alivePeers.next().foreach(peer => router.route(PeerDiscoveryActor.GetPeersFrom(peer), self))

    case PeerInfo(peer, peers) => {
      self ! Peers(peers)
      deadPeers.remove(peer)
      peerResponses.put(peer, peers) match {
        case Some(oldValue) if oldValue == peers => //nothing changed
        case _ if (peers -- deadPeers).nonEmpty  => broadcastPeerInfo(peer, peers -- deadPeers)
        case _                                   =>
      }
    }

    case PeerProblem(peer) => {
      println("PeerProblem")
      alivePeers.remove(peer)
      deadPeers.add(peer)
    }

    case WebSocketConnected(client) => {
      connections.add(client)
      client ! jsonPeersData
    }
  }

  private def jsonPeersData =
    peerResponses
      .foldLeft(Json.obj())((json, keyValue) => json + (keyValue._1.getHostString, JsArray(keyValue._2.map(v => JsString(v.getHostString)).toSeq)))
      .toString()

  private def broadcastPeerInfo(peer: InetSocketAddress, peers: Set[InetSocketAddress]): Unit = {
    val response = Json.obj(peer.getHostString -> JsArray(peers.map(p => JsString(p.getHostString)).toSeq)).toString()
    connections.foreach(c => c ! response)
  }
}

object MainActor {

  case class PeerInfo(peer: InetSocketAddress, peers: Set[InetSocketAddress])

  case class PeerProblem(peer: InetSocketAddress)

  case class Peers(peers: Set[InetSocketAddress])

  case class WebSocketConnected(actor: ActorRef)

  case object Discover

  def apply(chainId: Char, workersCount: Int)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(classOf[MainActor], chainId, workersCount))
  }
}
