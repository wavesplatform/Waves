package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.wavesplatform.Version
import org.scalatest.{FlatSpecLike, Matchers}
import scorex.app.ApplicationVersion
import scorex.network.{Handshake, PeerConnectionHandler}

class PeerRegisterSpecification extends TestKit(ActorSystem()) with ImplicitSender with FlatSpecLike with Matchers {
  val address1 = new InetSocketAddress("1.1.1.1", 1)
  val address2 = new InetSocketAddress("2.2.2.2", 2)
  val address3 = new InetSocketAddress("3.3.3.3", 3)

  val handshake1 = Handshake("wavesT", new ApplicationVersion(Version.VersionTuple), "node", 1, None, System.currentTimeMillis() / 1000)

  val handler1 = TestActorRef[PeerConnectionHandler]
  val handler2 = TestActorRef[PeerConnectionHandler]

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
    register.registerHandler(address1, handler1)
    register.registerHandshake(address1, handshake1)
    register.initiateOutboundConnection(address2)
    register.registerHandler(address2, handler2)
    register.initiateOutboundConnection(address3)

    register.isRegistered(address1) should be(true)
    register.isRegistered(address2) should be(true)
    register.isRegistered(address3) should be(false)
  }

  it should "properly report connection direction of connecting peer" in {
    val register = new PeerRegister
    register.initiateOutboundConnection(address1)
    register.registerHandler(address1, handler1)
    register.registerHandler(address2, handler2)

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
}
