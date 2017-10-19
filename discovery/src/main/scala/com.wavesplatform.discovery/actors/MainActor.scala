package com.wavesplatform.discovery.actors

import java.net.InetSocketAddress
import java.time.LocalDateTime

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorRef, OneForOneStrategy, Props, SupervisorStrategy}
import akka.routing.{ActorRefRoutee, Router, SmallestMailboxRoutingLogic}
import com.wavesplatform.discovery.collections.Pool
import play.api.libs.json._

class ExpirationSet[T](val expirationTimeMilis: Long) extends scala.collection.mutable.Set[T]{
  private var inner = Map.empty[T, Long]
  private def freshInner = {
    val time = System.currentTimeMillis()
    inner = inner.filter({ case (k, v) =>
      time - v > expirationTimeMilis })

    inner
  }

  override def +=(elem: T) = {
    inner += ((elem, System.currentTimeMillis()))
    this
  }

  override def -=(elem: T) = {
    inner = inner.-(elem)
    this
  }

  override def contains(elem: T) = freshInner.contains(elem)

  override def iterator = freshInner.keys.iterator
}

class MainActor extends Actor {

  import MainActor._

  var router = {
    val routees = Vector.fill(5) {
      val r = context.actorOf(Props[PeerDiscoveryActor])
      context watch r
      ActorRefRoutee(r)
    }
    Router(SmallestMailboxRoutingLogic(), routees)
  }

  private val alivePeers = new Pool[InetSocketAddress]
  private val deadPeers = new ExpirationSet[InetSocketAddress](1000*60*60*5)
  private val peerResponses = scala.collection.mutable.Map.empty[InetSocketAddress, Set[InetSocketAddress]]
  private val connections = scala.collection.mutable.Set.empty[ActorRef]

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
    case _: Exception => Restart
  }

  def receive: PartialFunction[Any, Unit] = {

    case Peers(p) => p.foreach(alivePeers.add)

    case Discover => alivePeers.next().foreach(peer => router.route(PeerDiscoveryActor.GetPeersFrom(peer), self))

    case PeerInfo(peer, peers) => {
      self ! Peers(peers)

      peerResponses.put(peer, peers) match {
        case Some(oldValue) if oldValue == peers => //nothing changed
        case _ if peers.nonEmpty => broadcastPeerInfo(peer, peers)
        case _ =>
      }
    }

    case PeerProblem(peer) => {
      alivePeers.remove(peer)
      deadPeers.add(peer)
    }

    case WebSocketConnected(client) => {
      connections.add(client)
      client ! jsonPeersData
    }
  }

  private def jsonPeersData = peerResponses.foldLeft(Json.obj())((json, keyValue) => json.+(keyValue._1.toString, JsArray(keyValue._2.map(v => JsString(v.toString)).toSeq))).toString()

  private def broadcastPeerInfo(peer: InetSocketAddress, peers: Set[InetSocketAddress]): Unit = {
    val response = Json.obj(peer.toString -> JsArray(peers.map(p => JsString(p.toString)).toSeq)).toString()
    connections.foreach(c => c ! response)
  }
}

object MainActor {

  case class PeerInfo(peer: InetSocketAddress, peers: Set[InetSocketAddress])

  case class PeerProblem(peer: InetSocketAddress)

  case class Peers(peers: Set[InetSocketAddress])

  case class WebSocketConnected(actor: ActorRef)

  case object Discover

}