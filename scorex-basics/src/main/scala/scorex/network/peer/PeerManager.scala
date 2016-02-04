package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.Actor
import scorex.app.Application
import scorex.network._
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.util.Random

/**
  * Must be singleton
  * @param application
  */
class PeerManager(application: Application) extends Actor with ScorexLogging {

  import PeerManager._

  private val connectedPeers = mutable.Map[ConnectedPeer, Option[Handshake]]()
  private var connectingPeer: Option[InetSocketAddress] = None

  private lazy val settings = application.settings
  private lazy val networkController = application.networkController

  private lazy val peerDatabase = new PeerDatabaseImpl(application)

  settings.knownPeers.foreach{address =>
    val defaultPeerInfo = PeerInfo(System.currentTimeMillis(), None, None)
    peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
  }

  private def randomPeer(): Option[InetSocketAddress] = {
    val peers = peerDatabase.knownPeers(true).keys.toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  //todo: combine AddKnownPeer & UpdatePeer?
  private def peerLists: Receive = {
    case AddKnownPeer(address) =>
      val peerInfo = PeerInfo(System.currentTimeMillis())
      peerDatabase.addOrUpdateKnownPeer(address, peerInfo)

    case UpdatePeer(address, peerNonce, peerName) =>
      val peerInfo = PeerInfo(System.currentTimeMillis(), peerNonce, peerName)
      peerDatabase.addOrUpdateKnownPeer(address, peerInfo)

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers(false).keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers(false).keys.toSeq).take(howMany)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.keys.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.flatten.toSeq: Seq[Handshake])

    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers(true)
  }

  private def peerCycle: Receive = {
    case Connected(newPeer@ConnectedPeer(remote, _)) =>
      connectedPeers += newPeer -> None
      if (connectingPeer.contains(remote)) {
        log.info(s"Connected to $remote")
        connectingPeer = None
      } else {
        log.info(s"Got incoming connection from $remote")
      }

    case Handshaked(address, handshake) =>
      val toUpdate = connectedPeers.filter { case (cp, h) =>
        cp.socketAddress == address || h.map(_.nodeNonce == handshake.nodeNonce).getOrElse(true)
      }

      if (toUpdate.isEmpty) {
        log.error("No peer to update")
      } else {
        val newCp = toUpdate
          .find(t => handshake.declaredAddress.contains(t._1.socketAddress))
          .getOrElse(toUpdate.head)
          ._1

        toUpdate.keys.foreach(connectedPeers.remove)

        //drop connection to self if occurred
        if (handshake.nodeNonce == application.settings.nodeNonce) {
          newCp.handlerRef ! PeerConnectionHandler.CloseConnection
          val peerNonce = Some(handshake.nodeNonce)
          val peerName = Some(handshake.nodeName)
          self ! UpdatePeer(handshake.declaredAddress.getOrElse(address), peerNonce, peerName)
        } else {
          handshake.declaredAddress.foreach(address => self ! PeerManager.AddKnownPeer(address))
          connectedPeers += newCp -> Some(handshake)
        }
      }

    case Disconnected(remote) =>
      connectedPeers.retain { case (p, _) => p.socketAddress != remote }
      if (connectingPeer.contains(remote)) {
        connectingPeer = None
      }
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections && connectingPeer.isEmpty) {
        randomPeer().foreach { address =>
          if (!connectedPeers.map(_._1.socketAddress).contains(address)) {
            connectingPeer = Some(address)
            networkController ! NetworkController.ConnectTo(address)
          }
        }
      }
  }: Receive) orElse peerLists orElse apiInterface orElse peerCycle
}

object PeerManager {

  case class AddKnownPeer(address: InetSocketAddress)

  case class UpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], peerName: Option[String])

  case object KnownPeers

  case object RandomPeer

  case class RandomPeers(hawMany: Int)

  case object CheckPeers

  case class Connected(newPeer: ConnectedPeer)

  case class Handshaked(address: InetSocketAddress, handshake: Handshake)

  case class Disconnected(remote: InetSocketAddress)

  case class FilterPeers(sendingStrategy: SendingStrategy)

  case object GetAllPeers

  case object GetConnectedPeers

}
