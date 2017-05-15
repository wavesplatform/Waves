package scorex.network.peer

import java.net.{InetAddress, InetSocketAddress}

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
import scorex.network.message._
import scorex.network.message.Message
import scorex.network.message.MessageHandler.RawNetworkData

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Left

class PeerManagerSpecification extends ActorTestingCommons {

  import PeerManager._

  val hostname = "localhost"

  val (address1, nonce1) = (new InetSocketAddress(hostname, 1), 1)
  val (address2, nonce2) = (new InetSocketAddress(hostname, 2), 2)

  val timeout: FiniteDuration = 3.seconds

  private val localConfig = ConfigFactory.parseString(
    """
      |waves {
      |  network {
      |    file: ""
      |    known-peers = ["127.0.0.1:1"]
      |    nonce: 123456789
      |    max-connections: 10
      |    black-list-residence-time: 1000
      |    peers-data-residence-time: 100s
      |    black-list-threshold: 2
      |  }
      |}
    """.stripMargin).withFallback(baseTestConfig).resolve()

  val wavesSettings: WavesSettings = WavesSettings.fromConfig(localConfig)

  protected override val actorRef: ActorRef = system.actorOf(PeerManager.props(wavesSettings.networkSettings,
    networkControllerMock, wavesSettings.blockchainSettings.addressSchemeCharacter))

  testSafely {

    val peerConnectionHandler = TestProbe("connection-handler")

    def connect(address: InetSocketAddress, nonce: Long): Unit = {
      actorRef ! Connected(address, peerConnectionHandler.ref, None, inbound = true)
      peerConnectionHandler.expectMsgType[Handshake]
      actorRef ! Handshaked(address, Handshake("wavesT", new ApplicationVersion(Version.VersionTuple), "", nonce, Some(address), 0))
    }

    def getConnectedPeers: Seq[(InetSocketAddress, Handshake)] =
      Await.result((actorRef ? GetConnectedPeers).mapTo[Seq[(InetSocketAddress, Handshake)]], testDuration)

    def getBlacklistedPeers: Set[String] =
      Await.result((actorRef ? GetBlacklistedPeers).mapTo[Set[String]], testDuration)

    def getActiveConnections: Seq[InetSocketAddress] =
      Await.result((actorRef ? GetConnections).mapTo[Seq[InetSocketAddress]], testDuration)

    def validatePeers(expectedSize: Int, expectedAddreses: Seq[InetSocketAddress], expectedNonceSeq: Seq[Long]): Unit = {
      val connectedPeers = getConnectedPeers
      connectedPeers.size shouldBe expectedSize
      connectedPeers.map(_._1) should contain theSameElementsAs expectedAddreses
      connectedPeers.map(_._2.nodeNonce) should contain theSameElementsAs expectedNonceSeq
    }

    "blacklisting" in {
      // connect to peer1
      actorRef ! CheckPeers
      networkController.expectMsg(NetworkController.ConnectTo(address1))
      connect(address1, nonce1)

      validatePeers(1, Seq(address1), Seq(nonce1))

      // adding peer1 to black list
      actorRef ! AddToBlacklist(address1)
      peerConnectionHandler.expectMsg(CloseConnection)

      actorRef ! Disconnected(address1)
      getActiveConnections shouldBe empty

      val t = wavesSettings.networkSettings.blackListResidenceTime / 2

      // trying to connect to peer
      actorRef ! CheckPeers

      // can't connect for half on blacklisting period
      networkController.expectNoMsg(t)

      // trying to connect to another peer on the same host (it also blacklisted)
      val secondPeerHandler = TestProbe("connection-handler-2")
      actorRef ! Connected(address2, secondPeerHandler.ref, null, inbound = true)

      secondPeerHandler.expectMsg(CloseConnection)
      secondPeerHandler.expectNoMsg(t)

      // After blacklisting period trying to establish outbound connection
      actorRef ! CheckPeers
      networkController.expectMsg(NetworkController.ConnectTo(address1))

      actorRef ! Connected(address2, secondPeerHandler.ref, null, inbound = true)
      secondPeerHandler.expectMsgType[Handshake]
    }

    "peer cycle" - {
      "connect - disconnect" in {
        connect(address2, nonce2)
        validatePeers(1, Seq(address2), Seq(nonce2))
        actorRef ! Disconnected(address2)
        validatePeers(0, Seq(), Seq())
      }

      "second connection from the same address is possible from another port" in {
        // connect to known peers, establishing outbound connection
        actorRef ! CheckPeers
        networkController.expectMsg(NetworkController.ConnectTo(address1))
        connect(address1, nonce1)

        validatePeers(1, Seq(address1), Seq(nonce1))

        // connect from second peer, imitate inbound connection
        connect(address2, nonce2)

        validatePeers(2, Seq(address1, address2), Seq(nonce1, nonce2))
      }

      "msg from network routing" - {
        println("=========================================================")
        val rawData = RawNetworkData(BlockMessageSpec, Array[Byte](23), address2)
        actorRef ! rawData

        def assertThatMessageGotten(expectedNonce: Long): Unit = networkController.expectMsgPF() {
          case Message(spec, Left(bytes), Some(p)) =>
            spec shouldEqual rawData.spec
            bytes shouldEqual rawData.data
            p.nonce shouldEqual expectedNonce
        }

        "msg is gotten" in {
          assertThatMessageGotten(nonce2)
        }

        "surviving reconnect" in {
          actorRef ! Disconnected(address2)

          val sameClientAnotherPort = new InetSocketAddress(hostname, address2.getPort + 2)
          connect(sameClientAnotherPort, nonce2)

          assertThatMessageGotten(nonce2)
        }
      }

      "msg to network routing" in {
        val p = mock[ConnectedPeer]
        p.nonce _ expects() returns nonce1

        val msg = Message(BlockMessageSpec, Left(Array[Byte](27)), None)
        actorRef ! SendToNetwork(msg, SendToChosen(p))

        peerConnectionHandler.expectMsg(msg)
      }

      "add to blacklist" in {
        actorRef ! AddToBlacklist(address2)

        getBlacklistedPeers should have size 1

        actorRef ! AddToBlacklist(new InetSocketAddress(address2.getHostName, address2.getPort + 1))

        getBlacklistedPeers should have size 1
        getBlacklistedPeers should contain(address2.getHostName)
      }
    }
    /*

    "connect to self is forbidden" in {
      connect(new InetSocketAddress("localhost", 45980), wavesSettings.networkSettings.firstNonce)
      peerConnectionHandler.expectMsg(CloseConnection)
      getActiveConnections shouldBe empty
    }

    "many TCP clients with same firstNonce from different host should connect" in {
      def connect(id: Int): TestProbe = {
        val address = new InetSocketAddress(s"$id.$id.$id.$id", id)
        val handler = TestProbe("connection-handler-" + id)
        actorRef ! Connected(address, handler.ref, None, inbound = true)
        handler.expectMsgType[Handshake](timeout)
        actorRef ! Handshaked(address, Handshake("wavesT", ApplicationVersion(0, 0, 0), "", firstNonce, None, 0))
        handler
      }

      val h1 = connect(1)
      h1.expectNoMsg()
      getConnectedPeers should have size 1

      val h2 = connect(2)
      h2.expectNoMsg()
      getConnectedPeers should have size 2

      val h3 = connect(3)
      h3.expectNoMsg()
      getConnectedPeers should have size 3
    }

    "many TCP clients from the same IP address" in {

      def connectNormal(id: Int): TestProbe = {
        val address = new InetSocketAddress(id)
        val handler = TestProbe("connection-handler-" + id)
        actorRef ! Connected(address, handler.ref, None, inbound = true)
        handler.expectMsgType[Handshake]
        actorRef ! Handshaked(address, Handshake("wavesT", ApplicationVersion(0, 0, 0), "", id, None, 0))
        handler
      }

      def connectOverLimit(id: Int): TestProbe = {
        val address = new InetSocketAddress(id)
        val handler = TestProbe("connection-handler-" + id)
        actorRef ! Connected(address, handler.ref, None, inbound = true)
        handler
      }

      val handlers: scala.collection.mutable.ListBuffer[TestProbe] = scala.collection.mutable.ListBuffer.empty[TestProbe]

      (1 to 5).foreach { i =>
        handlers += connectNormal(i)
        getConnectedPeers should have size i
      }

      (1 to 3).foreach { i =>
        val h = connectOverLimit(i + 5)
        h.expectMsg(CloseConnection)
        handlers += h
        getConnectedPeers should have size 5
      }
    }


    "disconnect during handshake" in {
      actorRef ! Connected(second, peerConnectionHandler.ref, None, inbound = true)
      peerConnectionHandler.expectMsgType[Handshake]

      getActiveConnections should have size 1

      actorRef ! Disconnected(second)
      getConnectedPeers shouldBe empty
      getActiveConnections shouldBe empty
    }

    "PeerManager returns on GetConnectedPeers list of pairs (InetSocketAddress, Handshake)" in {
      assert(getConnectedPeers.isEmpty)

      // prepare
      connect(second, 655)

      // assert
      val result2 = getConnectedPeers
      assert(result2.nonEmpty)
      val (a, h) = result2.head
      assert(a == second)
      assert(h.applicationName == "wavesT")
    }

    "get random peers" in {
      actorRef ! AddPeer(new InetSocketAddress(99))
      actorRef ! AddPeer(new InetSocketAddress(100))

      val address1 = new InetSocketAddress(101)
      actorRef ! AddPeer(address1)
      connect(address1, 11)

      val address2 = new InetSocketAddress(56099)
      connect(address2, 56099)

      val peers = Await.result((actorRef ? GetRandomPeersToBroadcast(3)).mapTo[Seq[InetSocketAddress]], testDuration)

      peers should have size 2
      peers should contain allOf(address1, address2)
    }

    "blacklist nonconnected peer" in {
      getBlacklistedPeers shouldBe empty

      actorRef ! AddToBlacklist(knownAddress)

      getBlacklistedPeers.size shouldBe 1
    }

    "inbound connection limit exceeded but can make outbound connections" in {
      def connect(id: Int, inbound: Boolean): TestProbe = {
        val address = new InetSocketAddress(InetAddress.getByName(s"127.0.0.$id"), id)
        val handler = TestProbe("inbound-connection-handler-" + id)
        actorRef ! Connected(address, handler.ref, None, inbound)

        handler
      }

      (1 to wavesSettings.networkSettings.maxInboundConnections).foreach { i =>
        val h = connect(i, true)
        h.expectMsgType[Handshake](15.seconds)
      }
      (1 to wavesSettings.networkSettings.maxInboundConnections).foreach { i =>
        val h1 = connect(100 + i, true)
        h1.expectMsg(timeout, CloseConnection)
        val h2 = connect(200 + i, false)
        h2.expectMsgType[Handshake](15.seconds)
      }
    }

    "blacklist suspected peer" in {
      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(knownAddress)
      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(knownAddress)
      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(knownAddress)

      getBlacklistedPeers.size shouldBe 1
    }

    "blacklisted peer should be removed from suspected" in {
      getBlacklistedPeers shouldBe empty

      actorRef ! Suspect(knownAddress)
      actorRef ! Suspect(knownAddress)
      actorRef ! Suspect(knownAddress)
      getBlacklistedPeers.size shouldBe 1

      Thread sleep 1100

      getBlacklistedPeers shouldBe empty
      actorRef ! Suspect(knownAddress)
      getBlacklistedPeers shouldBe empty
      actorRef ! Suspect(knownAddress)
      getBlacklistedPeers shouldBe empty
      actorRef ! Suspect(knownAddress)
      getBlacklistedPeers.size shouldBe 1
    }

    "don't allow more than one connection to the same ip-firstNonce pair" in {
      def connect(ip: InetAddress, port: Int, firstNonce: Long): TestProbe = {
        val address = new InetSocketAddress(ip, port)
        val handler = TestProbe()
        actorRef ! Connected(address, handler.ref, None, inbound = true)
        handler.expectMsgType[Handshake]
        actorRef ! Handshaked(address, Handshake("wavesT", ApplicationVersion(0, 0, 0), "", firstNonce, None, 0))
        handler
      }

      val ip = InetAddress.getByName("localhost")
      val h1 = connect(ip, 1, 1)
      getConnectedPeers should have size 1
      val h2 = connect(ip, 2, 1)
      getConnectedPeers should have size 1
      h2.expectMsg(CloseConnection)
    }
  */
  }
}
