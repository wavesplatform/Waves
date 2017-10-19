package com.wavesplatform.discovery.actors

import java.net.InetSocketAddress
import java.util.concurrent.TimeUnit

import akka.actor.Actor

import scala.concurrent.Await
import scala.concurrent.duration._
import com.wavesplatform.discovery._

object PeerDiscoveryActor {
  case class GetPeersFrom(peer: InetSocketAddress)
}

class PeerDiscoveryActor extends Actor {
  import PeerDiscoveryActor._
  def receive: PartialFunction[Any, Unit] = {
    case GetPeersFrom(peer) => context.parent ! MainActor.PeerInfo(peer, Await.result(getPeersFromNode(peer), FiniteDuration(1, TimeUnit.MINUTES)))
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    message match {
      case Some(GetPeersFrom(peer)) => context.parent ! MainActor.PeerInfo(peer, Set.empty)
      case _ =>
    }
  }
}