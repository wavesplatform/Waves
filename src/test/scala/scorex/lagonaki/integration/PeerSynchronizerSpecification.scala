package scorex.lagonaki.integration

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.testkit._
import akka.util.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{Matchers, WordSpecLike}
import scorex.lagonaki.TestingCommons
import scorex.network.NetworkController.DataFromPeer
import scorex.network.message.Message
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager.{KnownPeers, RandomPeers}
import scorex.network.{ConnectedPeer, PeerSynchronizer}

class PeerSynchronizerSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with ScalaFutures
  with TestingCommons {

  import application.basicMessagesSpecsRepo._

  def this() = this(ActorSystem("PeerSynchronizerSpecification"))

  val probe = new TestProbe(system)

  val ps = system.actorOf(Props(classOf[PeerSynchronizer], application))

  val peer = ConnectedPeer(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 1), probe.ref)

  implicit val config = PatienceConfig(Span(2, Seconds), Span(5, Millis))

  implicit val timeout = Timeout(5, TimeUnit.SECONDS)

  "PeerSynchronizer actor" must {
    "response with known peers" in {
      ps ! DataFromPeer(GetPeersSpec.messageCode, Right, peer)
      val peers = (application.peerManager ? RandomPeers(3))
        .mapTo[Seq[InetSocketAddress]]
        .futureValue

      peers.length shouldBe 1
      val msg = Message(PeersSpec, Right(peers), None)
      probe.expectMsg(msg)

      val newPeer = new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 2)
      application.peerManager ! PeerManager.AddOrUpdatePeer(newPeer, None, None)

      val newPeers = (application.peerManager ? RandomPeers(3))
        .mapTo[Seq[InetSocketAddress]]
        .futureValue
      newPeers.length shouldBe 2
      ps ! DataFromPeer(GetPeersSpec.messageCode, Right, peer)
      probe.expectMsgPF() {
        case Message(spec, Right(p: Seq[InetSocketAddress]), None)
          if spec == PeersSpec && p.toSet == newPeers.toSet =>
      }
    }

    "add more peers" in {
      val peersBefore = (application.peerManager ? KnownPeers)
        .mapTo[Seq[InetSocketAddress]]
        .futureValue

      peersBefore.length shouldBe 2
      val newPeer = new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 3)
      val peers: Seq[InetSocketAddress] = Seq(newPeer)
      ps ! DataFromPeer(PeersSpec.messageCode, peers, peer)
      ps ! DataFromPeer(GetPeersSpec.messageCode, Right, peer)
      probe.expectMsgPF() {
        case Message(spec, Right(p: Seq[InetSocketAddress]), None)
          if spec == PeersSpec && p.toSet == peersBefore.toSet + newPeer =>
      }
    }
  }
}