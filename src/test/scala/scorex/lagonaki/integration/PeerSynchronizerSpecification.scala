package scorex.lagonaki.integration

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{ActorSystem, Props}
import akka.testkit._
import org.scalatest.{Matchers, WordSpecLike}
import scorex.lagonaki.TestingCommons
import scorex.network.NetworkController.DataFromPeer
import scorex.network.message.Message
import scorex.network.{ConnectedPeer, PeerSynchronizer}

//TODO move to basics
class PeerSynchronizerSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with TestingCommons {

  import TestingCommons._
  import application.basicMessagesSpecsRepo._

  def this() = this(ActorSystem("PeerSynchronizerSpecification"))

  val probe = new TestProbe(system)

  val ps = system.actorOf(Props(classOf[PeerSynchronizer], application))

  val peer = new ConnectedPeer(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 1), probe.ref)

  "PeerSynchronizer actor" must {
    "response with known peers" in {
      ps ! DataFromPeer(GetPeersSpec.messageCode, Right(), peer)
      val peers = application.peerManager.knownPeers().take(3)
      peers.length shouldBe 1
      val msg = Message(PeersSpec, Right(peers), None)
      probe.expectMsg(msg)

      val newPeer = new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 2)
      application.peerManager.addPeer(newPeer)
      val newPeers = application.peerManager.knownPeers().take(3)
      newPeers.length shouldBe 2
      ps ! DataFromPeer(GetPeersSpec.messageCode, Right(), peer)
      probe.expectMsg(Message(PeersSpec, Right(newPeers), None))
    }

    "add more peers" in {
      val peersBefore = application.peerManager.knownPeers()
      peersBefore.length shouldBe 2
      val newPeer = new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 3)
      val peers: Seq[InetSocketAddress] = Seq(newPeer)
      ps ! DataFromPeer(PeersSpec.messageCode, peers, peer)
      ps ! DataFromPeer(GetPeersSpec.messageCode, Right(), peer)
      probe.expectMsg(Message(PeersSpec, Right(peersBefore ++ Seq(newPeer)), None))
    }
  }
}

