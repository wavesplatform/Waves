package scorex.network.peer

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.Props
import akka.pattern.ask
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.WavesSettings
import scorex.ActorTestingCommons
import scorex.app.ApplicationVersion
import scorex.network.NetworkController.SendToNetwork
import scorex.network.PeerConnectionHandler.CloseConnection
import scorex.network._
import scorex.network.message.Message
import scorex.network.message.MessageHandler.RawNetworkData

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Left

class PeerManagerSpecification extends ActorTestingCommons {

  import PeerManager._

  val hostname = "localhost"
  val knownAddress = new InetSocketAddress(hostname, 6789)

  val timeout = 6.seconds

  private val localConfig = ConfigFactory.parseString(
    """
      |waves {
      |  network {
      |    file: ""
      |    known-peers = ["127.0.0.1:6789"]
      |    nonce: 123456789
      |    max-connections: 10
      |    black-list-residence-time: 1000
      |    peers-data-residence-time: 100s
      |    black-list-threshold: 2
      |  }
      |}
    """.stripMargin).withFallback(baseTestConfig).resolve()

  val wavesSettings = WavesSettings.fromConfig(localConfig)

  trait App extends ApplicationMock {
    override lazy val settings = wavesSettings
    override val applicationName: String = "test"
    override val appVersion: ApplicationVersion = ApplicationVersion(7, 7, 7)
  }

  private val app = stub[App]

  import app.basicMessagesSpecsRepo._

  protected override val actorRef = system.actorOf(Props(classOf[PeerManager], app))

  testSafely {

    val nonce = 777

    val peerConnectionHandler = TestProbe("connection-handler")

    def connect(address: InetSocketAddress, noneNonce: Long): Unit = {
      actorRef ! Connected(address, peerConnectionHandler.ref, None, inbound = true)
      peerConnectionHandler.expectMsgType[Handshake]
      actorRef ! Handshaked(address, Handshake("test", ApplicationVersion(0, 0, 0), "", noneNonce, Some(address), 0))
    }

    def getConnectedPeers =
      Await.result((actorRef ? GetConnectedPeers).mapTo[Seq[(InetSocketAddress, Handshake)]], testDuration)

    def getBlacklistedPeers =
      Await.result((actorRef ? GetBlacklistedPeers).mapTo[Set[String]], testDuration)

    def getActiveConnections =
      Await.result((actorRef ? GetConnections).mapTo[Seq[InetSocketAddress]], testDuration)

    val anAddress = new InetSocketAddress(hostname, knownAddress.getPort + 1)

    "blacklisting" in {
      actorRef ! CheckPeers
      networkController.expectMsg(NetworkController.ConnectTo(knownAddress))
      connect(knownAddress, nonce)

      actorRef ! AddToBlacklist(knownAddress)
      peerConnectionHandler.expectMsg(CloseConnection)

      actorRef ! Disconnected(knownAddress)
      getActiveConnections shouldBe empty

      val t = wavesSettings.networkSettings.blackListResidenceTime / 2

      actorRef ! CheckPeers

      networkController.expectNoMsg(t)

      val anotherAddress = new InetSocketAddress(knownAddress.getHostName, knownAddress.getPort + 1)
      val anotherPeerHandler = TestProbe("connection-handler-2")

      actorRef ! Connected(anotherAddress, anotherPeerHandler.ref, null, inbound = true)

      anotherPeerHandler.expectMsg(CloseConnection)
      anotherPeerHandler.expectNoMsg(t)

      actorRef ! CheckPeers
      networkController.expectMsg(NetworkController.ConnectTo(knownAddress))

      actorRef ! Connected(anotherAddress, anotherPeerHandler.ref, null, inbound = true)
      anotherPeerHandler.expectMsgType[Handshake]
    }

    "peer cycle" - {
      connect(anAddress, nonce)

      val connectedPeers = getConnectedPeers
      val (addr, Handshake(_, _, _, nodeNonce, _, _)) = connectedPeers.head

      "connect - disconnect" in {
        connectedPeers.size shouldBe 1

        addr shouldEqual anAddress
        nodeNonce shouldEqual nonce

        actorRef ! Disconnected(anAddress)
        getConnectedPeers shouldBe empty
      }

      "second connection from the same address is possible from another port" in {
        actorRef ! CheckPeers
        networkController.expectMsg(NetworkController.ConnectTo(knownAddress))
        connect(knownAddress, nonce)
        peerConnectionHandler.expectMsg(PeerConnectionHandler.CloseConnection)
        getConnectedPeers.size shouldBe 1

        actorRef ! Disconnected(anAddress)
        getConnectedPeers.size shouldBe 0

        actorRef ! CheckPeers
        networkController.expectMsgType[NetworkController.ConnectTo]
        connect(knownAddress, nonce)
        getConnectedPeers.size shouldBe 1

        getConnectedPeers.head._1 shouldBe knownAddress
        getActiveConnections.head shouldBe knownAddress
      }

      "msg from network routing" - {
        val rawData = RawNetworkData(BlockMessageSpec, Array[Byte](23), anAddress)
        actorRef ! rawData

        def assertThatMessageGotten(): Unit = networkController.expectMsgPF() {
          case Message(spec, Left(bytes), Some(p)) =>
            spec shouldEqual rawData.spec
            bytes shouldEqual rawData.data
            p.nonce shouldEqual nodeNonce
        }

        "msg is gotten" in {
          assertThatMessageGotten()
        }

        "surviving reconnect" in {
          actorRef ! Disconnected(anAddress)

          val sameClientAnotherPort = new InetSocketAddress(hostname, knownAddress.getPort + 2)
          connect(sameClientAnotherPort, nonce)

          assertThatMessageGotten()
        }
      }

      "msg to network routing" in {
        val p = mock[ConnectedPeer]
        p.nonce _ expects() returns nonce

        val msg = Message(BlockMessageSpec, Left(Array[Byte](27)), None)
        actorRef ! SendToNetwork(msg, SendToChosen(p))

        peerConnectionHandler.expectMsg(msg)
      }

      "add to blacklist" in {
        actorRef ! AddToBlacklist(anAddress)

        getBlacklistedPeers should have size 1

        actorRef ! AddToBlacklist(new InetSocketAddress(anAddress.getHostName, anAddress.getPort + 1))

        getBlacklistedPeers should have size 1
        getBlacklistedPeers should contain(anAddress.getHostName)
      }
    }

    "connect to self is forbidden" in {
      connect(new InetSocketAddress("localhost", 45980), wavesSettings.networkSettings.nonce)
      peerConnectionHandler.expectMsg(CloseConnection)
      getActiveConnections shouldBe empty
    }

    "many TCP clients with same nonce from different host should connect" in {
      def connect(id: Int): TestProbe = {
        val address = new InetSocketAddress(s"$id.$id.$id.$id", id)
        val handler = TestProbe("connection-handler-" + id)
        actorRef ! Connected(address, handler.ref, None, inbound = true)
        handler.expectMsgType[Handshake](timeout)
        actorRef ! Handshaked(address, Handshake("test", ApplicationVersion(0, 0, 0), "", nonce, None, 0))
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
        actorRef ! Handshaked(address, Handshake("test", ApplicationVersion(0, 0, 0), "", id, None, 0))
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
      actorRef ! Connected(anAddress, peerConnectionHandler.ref, None, inbound = true)
      peerConnectionHandler.expectMsgType[Handshake]

      getActiveConnections should have size 1

      actorRef ! Disconnected(anAddress)
      getConnectedPeers shouldBe empty
      getActiveConnections shouldBe empty
    }

    "PeerManager returns on GetConnectedPeers list of pairs (InetSocketAddress, Handshake)" in {
      assert(getConnectedPeers.isEmpty)

      // prepare
      connect(anAddress, 655)

      // assert
      val result2 = getConnectedPeers
      assert(result2.nonEmpty)
      val (a, h) = result2.head
      assert(a == anAddress)
      assert(h.applicationName == "test")
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
        val handler = TestProbe("connection-handler-" + id)
        actorRef ! Connected(address, handler.ref, None, inbound)

        handler
      }

      (1 to wavesSettings.networkSettings.maxConnections).foreach { i =>
        val h = connect(i, true)
        h.expectMsgType[Handshake](15.seconds)
      }
      (1 to wavesSettings.networkSettings.maxConnections).foreach { i =>
        val h1 = connect(i, true)
        h1.expectMsg(timeout, CloseConnection)
        val h2 = connect(100 + i, false)
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

    "don't allow more than one connection to the same ip-nonce pair" in {
      def connect(ip: InetAddress, port: Int, nonce: Long): TestProbe = {
        val address = new InetSocketAddress(ip, port)
        val handler = TestProbe()
        actorRef ! Connected(address, handler.ref, None, inbound = true)
        handler.expectMsgType[Handshake]
        actorRef ! Handshaked(address, Handshake("test", ApplicationVersion(0, 0, 0), "", nonce, None, 0))
        handler
      }

      val ip = InetAddress.getByName("localhost")
      val h1 = connect(ip, 1, 1)
      getConnectedPeers should have size 1
      val h2 = connect(ip, 2, 1)
      getConnectedPeers should have size 1
      h2.expectMsg(CloseConnection)
    }
  }
}
