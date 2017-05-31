package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import com.wavesplatform.Version
import org.scalatest.{FlatSpecLike, Matchers}
import scorex.app.ApplicationVersion
import scorex.network.Handshake

class PeerRegisterSpecification extends TestKit(ActorSystem("PeerRegisterSpecification")) with FlatSpecLike with Matchers {
  val address1 = new InetSocketAddress("1.1.1.1", 1)
  val address2 = new InetSocketAddress("2.2.2.2", 2)
  val address3 = new InetSocketAddress("3.3.3.3", 3)
  val google = new InetSocketAddress("8.8.4.4", 80)

  val handshake1 = Handshake("wavesT", new ApplicationVersion(Version.VersionTuple), "node1", 1, None, System.currentTimeMillis() / 1000)
  val handshake2 = Handshake("wavesT", new ApplicationVersion(Version.VersionTuple), "node2", 2, Some(address2), System.currentTimeMillis() / 1000)
  val handshake = Handshake("wavesT", new ApplicationVersion(Version.VersionTuple), "google", 3, Some(google), System.currentTimeMillis() / 1000)

  "PeerRegister" should "support outgoing connection during establishing connection" in {
    val register = new PeerRegister

    register.initiateOutboundConnection(address1)
    register.initiateOutboundConnection(address2)
    register.getStageOfAddress(address1) should be(ConnectingPeer)
    register.getStageOfAddress(address2) should be(ConnectingPeer)
    register.getStageOfAddress(address3) should be(UnknownPeer)
  }

  it should "properly report registered addresses" in {
    val register = new PeerRegister
    register.initiateOutboundConnection(address1)
    register.registerHandler(address1, null)
    register.registerHandshake(address1, handshake1)
    register.initiateOutboundConnection(address2)
    register.registerHandler(address2, null)
    register.initiateOutboundConnection(address3)

    register.getStageOfAddress(address1) should be(HandshakedPeer)
    register.getStageOfAddress(address2) should be(ConnectedPeer)
    register.getStageOfAddress(address3) should be(ConnectingPeer)
  }

  it should "properly report connection direction of connecting peer" in {
    val register = new PeerRegister
    register.initiateOutboundConnection(address1)
    register.registerHandler(address1, null)
    register.registerHandler(address2, null)

    register.isConnectionInbound(address1) should be(Right(false))
    register.isConnectionInbound(address2) should be(Right(true))
    register.isConnectionInbound(address3) should be(Left("Unregistered address '/3.3.3.3:3'"))
  }

  it should "correctly reply with presence of peers in the middle of connection establishing process" in {
    val register = new PeerRegister
    register.initiateOutboundConnection(address1)
  }

  it should "return empty set for handshaked addresses" in {
    val register = new PeerRegister

    register.handshakedAddresses should be(empty)
  }

  it should "return peer by host" in {
    val register = new PeerRegister
    register.initiateOutboundConnection(address1)
    val handler1 = TestProbe().ref
    register.registerHandler(address1, handler1)
    register.registerHandshake(address1, handshake1)


    val handler2 = TestProbe().ref
    register.registerHandler(google, handler2)
    register.registerHandshake(google, handshake)

    register.getConnectionHandlersByHost(address1.getAddress) should be(Seq(handler1))
    register.getConnectionHandlersByHost(google.getAddress) should be(Seq(handler2))
    register.getConnectionHandlersByHost(address2.getAddress) should be (Seq.empty)
  }
}
