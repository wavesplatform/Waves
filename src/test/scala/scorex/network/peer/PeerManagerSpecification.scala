package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.ActorRef
import akka.pattern.ask
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import com.wavesplatform.Version
import com.wavesplatform.settings.WavesSettings
import scorex.ActorTestingCommons
import scorex.app.ApplicationVersion
import scorex.network.NetworkController.SendToNetwork
import scorex.network.PeerConnectionHandler.CloseConnection
import scorex.network._
import scorex.network.message.MessageHandler.RawNetworkData
import scorex.network.message.{Message, _}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Left

class PeerManagerSpecification extends ActorTestingCommons {

  import PeerManager._

  val hostname = "localhost"

  val (address1, nonce1) = (new InetSocketAddress(hostname, 1), 1)
  val (address2, nonce2) = (new InetSocketAddress(hostname, 2), 2)

  val timeout: FiniteDuration = 1.seconds

  private val localConfig = ConfigFactory.parseString(
    """
      |waves {
      |  network {
      |    file = ""
      |    known-peers = ["127.0.0.1:1"]
      |    nonce = 123456789
      |    max-inbound-connections = 5
      |    max-outbound-connections = 5
      |    black-list-residence-time = 1000
      |    peers-data-residence-time = 100s
      |    black-list-threshold = 2
      |  }
      |}
    """.stripMargin).withFallback(baseTestConfig).resolve()

  val wavesSettings: WavesSettings = WavesSettings.fromConfig(localConfig)

  protected override val actorRef: ActorRef = system.actorOf(PeerManager.props(wavesSettings.networkSettings,
    networkControllerMock, wavesSettings.blockchainSettings.addressSchemeCharacter))

  private def getConnectedPeers: Seq[(InetSocketAddress, Handshake)] =
    Await.result((actorRef ? GetConnectedPeers).mapTo[Seq[(InetSocketAddress, Handshake)]], testDuration)

  private def getBlacklistedPeers: Set[String] =
    Await.result((actorRef ? GetBlacklistedPeers).mapTo[Set[String]], testDuration)

  private def validatePeers(expectedSize: Int, expectedAddreses: Seq[InetSocketAddress], expectedNonceSeq: Seq[Long]): Unit = {
    val connectedPeers = getConnectedPeers
    connectedPeers.size shouldBe expectedSize
    connectedPeers.map(_._1) should contain theSameElementsAs expectedAddreses
    connectedPeers.map(_._2.nodeNonce) should contain theSameElementsAs expectedNonceSeq
  }

  private def imitateConnection(address: InetSocketAddress, nonce: Long): TestProbe = {
    val handler = TestProbe(s"connection-handler-to-${address.getHostName}:${address.getPort}-nonce-$nonce")
    actorRef ! Connected(address, handler.ref, None)
    handler.expectMsgType[Handshake]
    actorRef ! Handshaked(address, Handshake("wavesT", new ApplicationVersion(Version.VersionTuple), "", nonce, Some(address), 0))

    handler
  }

  private def imitateInboundConnection(n: Int): TestProbe = imitateConnection(new InetSocketAddress(s"$n.$n.$n.$n", n), n)

  private def initiateInboundConnection(address: InetSocketAddress): TestProbe = {
    val handler = TestProbe(s"connection-handler-to-${address.getHostName}:${address.getPort}")
    actorRef ! Connected(address, handler.ref, None)

    handler
  }

  private def initiateOutboundConnection(address: InetSocketAddress): TestProbe = {
    actorRef ! CheckPeers
    networkController.expectMsgType[NetworkController.ConnectTo]

    initiateInboundConnection(address)
  }

  private def createAddress(n: Int): InetSocketAddress = new InetSocketAddress(s"$n.$n.$n.$n", n)

  private def blockNetworkMessage(address: InetSocketAddress): RawNetworkData =
    RawNetworkData(BlockMessageSpec, Array[Byte](BlockMessageSpec.messageCode), address)

  private def assertThatMessageGotten(expectedMessage: RawNetworkData, expectedNonce: Long): Unit = networkController.expectMsgPF() {
    case Message(spec, Left(bytes), Some(source)) =>
      spec shouldEqual expectedMessage.spec
      bytes shouldEqual expectedMessage.data
      source.nonce shouldEqual expectedNonce
  }


  // WARNING!!!
  // Every test path executed separately
  testSafely {

    "blacklisting" in {
      // connect to peer1
      actorRef ! CheckPeers
      networkController.expectMsg(NetworkController.ConnectTo(address1))
      val peer1ConnectionHandler = imitateConnection(address1, nonce1)

      validatePeers(1, Seq(address1), Seq(nonce1))

      // adding peer1 to black list
      actorRef ! AddToBlacklist(address1)
      peer1ConnectionHandler.expectMsg(CloseConnection)

      actorRef ! Disconnected(address1)
      getConnectedPeers shouldBe empty

      getBlacklistedPeers should have size 1

      // Trying to add the peer on the same host but different port
      actorRef ! AddToBlacklist(new InetSocketAddress(address2.getHostName, address2.getPort))

      // Host should be blacklisted
      getBlacklistedPeers should have size 1
      getBlacklistedPeers should contain(address1.getHostName)

      val t = wavesSettings.networkSettings.blackListResidenceTime / 2

      // trying to connect to peer
      actorRef ! CheckPeers

      // can't connect for half on blacklisting period
      networkController.expectNoMsg(t)

      // trying to connect to another peer on the same host (it also blacklisted), should forbid to connect
      val peer2ConnectionHandler = TestProbe("connection-to-peer-2")
      actorRef ! Connected(address2, peer2ConnectionHandler.ref, None)

      peer2ConnectionHandler.expectMsg(CloseConnection)
      peer2ConnectionHandler.expectNoMsg(t)

      // after blacklisting period trying to establish outbound connection, it should succeed
      actorRef ! CheckPeers
      networkController.expectMsg(NetworkController.ConnectTo(address1))

      actorRef ! Connected(address2, peer2ConnectionHandler.ref, None)
      peer2ConnectionHandler.expectMsgType[Handshake]
    }

    "blacklist of not connected peer should work" in {
      getBlacklistedPeers shouldBe empty

      actorRef ! AddToBlacklist(address1)

      getBlacklistedPeers.size shouldBe 1
    }

    "peer suspected more twice (as in settings) should be blacklisted" in {
      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(address1)
      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(address1)

      val blacklisted = getBlacklistedPeers
      blacklisted.size shouldBe 1
      blacklisted should contain(address1.getHostName)
    }

    "blacklisted peer should be removed from suspected" in {
      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(address1)
      actorRef ! Suspect(address1)
      getBlacklistedPeers.size shouldBe 1

      Thread.sleep(wavesSettings.networkSettings.blackListResidenceTime.toMillis * 2)

      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(address1)
      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(address1)
      getBlacklistedPeers.size shouldBe 1
    }

    "peer cycle" in {
      // Establish inbound connection from peer1
      imitateConnection(address1, nonce1)

      validatePeers(1, Seq(address1), Seq(nonce1))
      // Close inbound connections from peer1
      actorRef ! Disconnected(address1)
      validatePeers(0, Seq(), Seq())

      // Establish outbound connection to know peers (at this time we know only peer1)
      actorRef ! CheckPeers
      imitateConnection(address1, nonce1)
      networkController.expectMsg(NetworkController.ConnectTo(address1))

      validatePeers(1, Seq(address1), Seq(nonce1))

      // Establish inbound connection from peer2 (which is on the same host but on different port), it should be possible
      imitateConnection(address2, nonce2)
      validatePeers(2, Seq(address1, address2), Seq(nonce1, nonce2))
    }

    "network message could not be received from not connected peer" in {
      // Receive network message from not connected peer
      actorRef ! blockNetworkMessage(address1)
      networkController.expectNoMsg()
    }

    "network message could be received from connected peer" in {
      // Establish inbound connection from peer1
      imitateConnection(address1, nonce1)

      // Network message from peer2
      val message = blockNetworkMessage(address1)
      actorRef ! message
      assertThatMessageGotten(message, nonce1)
    }

    "network message could be received after reconnecting of source peer" in {
      // Establish inbound connection from peer2
      imitateConnection(address2, nonce2)
      val message = blockNetworkMessage(address2)

      actorRef ! message

      actorRef ! Disconnected(address2)

      val sameClientAnotherPort = new InetSocketAddress(address2.getHostName, address2.getPort + 2)
      imitateConnection(sameClientAnotherPort, nonce2)

      assertThatMessageGotten(message, nonce2)
    }

    "network message could be sent to peer" in {
      val peerConnectionHandler = imitateConnection(address1, nonce1)

      val p = mock[ConnectedPeer]
      p.nonce _ expects() returns nonce1

      val msg = Message(BlockMessageSpec, Left(Array[Byte](27)), None)
      actorRef ! SendToNetwork(msg, SendToChosen(p))

      peerConnectionHandler.expectMsg(msg)
    }

    "connect to self is forbidden" in {
      // Establish outbound connection to itself
      val handler = imitateConnection(new InetSocketAddress("localhost", 45980), wavesSettings.networkSettings.nonce)
      handler.expectMsg(CloseConnection)
      getConnectedPeers shouldBe empty
    }

    "many peers with the same nonce from different hosts should connect" in {
      val h1 = imitateInboundConnection(1)
      h1.expectNoMsg()
      getConnectedPeers should have size 1

      val h2 = imitateInboundConnection(2)
      h2.expectNoMsg()
      getConnectedPeers should have size 2

      val h3 = imitateInboundConnection(3)
      h3.expectNoMsg()
      getConnectedPeers should have size 3
    }

    "limit of inbound connections should work" in {
      def connectOverLimit(id: Int): TestProbe = {
        val address = new InetSocketAddress(id)
        val handler = TestProbe("connection-handler-" + id)
        actorRef ! Connected(address, handler.ref, None)

        handler
      }

      val handlers: scala.collection.mutable.ListBuffer[TestProbe] = scala.collection.mutable.ListBuffer.empty[TestProbe]
      (1 to 5).foreach { i =>
        handlers += imitateInboundConnection(i)
        getConnectedPeers should have size i
      }

      (1 to 3).foreach { i =>
        val h = connectOverLimit(i + 5)
        h.expectMsg(CloseConnection)
        handlers += h
        getConnectedPeers should have size 5
      }
    }

    "disconnect during handshake won't lead to new connection" in {
      val handler = TestProbe()
      actorRef ! Connected(address2, handler.ref, None)
      handler.expectMsgType[Handshake]
      getConnectedPeers shouldBe empty

      actorRef ! Disconnected(address2)
      getConnectedPeers shouldBe empty
    }

    "PeerManager returns on GetConnectedPeers list of handshaked connections" in {
      assert(getConnectedPeers.isEmpty)

      imitateConnection(address2, nonce2)

      val result2 = getConnectedPeers
      assert(result2.nonEmpty)

      val (a, h) = result2.head
      assert(a == address2)
      assert(h.applicationName == "wavesT")
    }

    "get random peers should work" in {
      // Adding few peers but not connecting them, they should disappear after timeout
      actorRef ! AddPeer(new InetSocketAddress(99))
      actorRef ! AddPeer(new InetSocketAddress(100))

      // Adding 2 peers and connecting them
      val a1 = new InetSocketAddress(101)
      actorRef ! AddPeer(a1)
      imitateConnection(a1, 11)

      val a2 = new InetSocketAddress(56099)
      imitateConnection(a2, 56099)

      val peers = Await.result((actorRef ? GetRandomPeersToBroadcast(3)).mapTo[Seq[InetSocketAddress]], testDuration)

      // Last two peers should be present in list
      peers should have size 2
      peers should contain allOf(a1, a2)
    }

    "inbound connection limit exceeded but can make outbound connections" in {
      // Can make inbound connections
      (1 to wavesSettings.networkSettings.maxInboundConnections).foreach { i =>
        val address = createAddress(i)
        initiateInboundConnection(address).expectMsgType[Handshake]
        actorRef ! Handshaked(address,
          Handshake("wavesT", new ApplicationVersion(Version.VersionTuple), "", i, Some(address), 0))
      }

      getConnectedPeers.size should be(wavesSettings.networkSettings.maxInboundConnections)

      initiateInboundConnection(createAddress(0)).expectMsg(timeout, CloseConnection)

      initiateOutboundConnection(address1).expectMsgType[Handshake]
      actorRef ! Handshaked(address1, Handshake("wavesT", new ApplicationVersion(Version.VersionTuple), "", nonce1,
        Some(address1), 0))

      getConnectedPeers.size should be(wavesSettings.networkSettings.maxInboundConnections + 1)
    }

    "don't allow more than one connection from the same ip-nonce pair" in {
      imitateConnection(address1, 1)
      getConnectedPeers should have size 1

      imitateConnection(address2, 1).expectMsg(CloseConnection)
      getConnectedPeers should have size 1
    }

  }
}
