package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.Props
import akka.io.Tcp.{Close, Connected}
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.network.peer.PeerManager

class NetworkListenerSpecification extends ActorTestingCommons {

  private val localhost = InetAddress.getByName("127.0.0.1")
  private val testBindAddress = new InetSocketAddress(localhost, 12300)
  private val testPeerManager = TestProbe("peerManager")
  private val testTcp = TestProbe()

  private val address1 = new InetSocketAddress(InetAddress.getByName("1.1.1.1"), 12301)
  private val address2 = new InetSocketAddress(InetAddress.getByName("2.2.2.2"), 12302)
  private val address3 = new InetSocketAddress(InetAddress.getByName("3.3.3.3"), 12303)

  protected override val actorRef = system.actorOf(Props(classOf[NetworkListener], networkController.ref,
    testPeerManager.ref, testBindAddress))

  testSafely {
    "NetworkListener" - {
      "should initialize blacklist" in {
        actorRef ! PeerManager.ExistedBlacklist(Seq("1.1.1.1", "2.2.2.2"))
        networkController.expectMsg(NetworkController.ReadyToListen)

        testTcp.send(actorRef, Connected(address1, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
        testTcp.send(actorRef, Connected(address2, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
        testTcp.send(actorRef, Connected(address3, testBindAddress))
        testTcp.expectNoMsg()
        networkController.expectMsgType[NetworkController.InboundConnection]
      }

      "should update initialized blacklist" in {
        actorRef ! PeerManager.ExistedBlacklist(Seq("1.1.1.1", "2.2.2.2"))
        networkController.expectMsg(NetworkController.ReadyToListen)
        actorRef ! PeerManager.BlackListUpdated("3.3.3.3")

        testTcp.send(actorRef, Connected(address1, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
        testTcp.send(actorRef, Connected(address2, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
        testTcp.send(actorRef, Connected(address3, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
      }

      "should reset blacklist" in {
        actorRef ! PeerManager.ExistedBlacklist(Seq("1.1.1.1", "2.2.2.2"))
        networkController.expectMsg(NetworkController.ReadyToListen)

        testTcp.send(actorRef, Connected(address1, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
        testTcp.send(actorRef, Connected(address2, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
        testTcp.send(actorRef, Connected(address3, testBindAddress))
        testTcp.expectNoMsg()
        networkController.expectMsgType[NetworkController.InboundConnection]

        actorRef ! PeerManager.ExistedBlacklist(Seq("2.2.2.2", "3.3.3.3"))
        testTcp.send(actorRef, Connected(address1, testBindAddress))
        testTcp.expectNoMsg()
        networkController.expectMsgType[NetworkController.InboundConnection]
        testTcp.send(actorRef, Connected(address2, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
        testTcp.send(actorRef, Connected(address3, testBindAddress))
        testTcp.expectMsg(Close)
        networkController.expectNoMsg()
      }
    }
  }

}
