package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.Props
import akka.pattern.ask
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.app.ApplicationVersion
import scorex.network.NetworkController.SendToNetwork
import scorex.network.PeerConnectionHandler.CloseConnection
import scorex.network._
import scorex.network.message.Message
import scorex.network.message.MessageHandler.RawNetworkData
import scorex.settings.SettingsMock

import scala.concurrent.Await
import scala.language.postfixOps
import scala.util.Left

class PeerManagerSpecification extends ActorTestingCommons {

  import PeerManager._

  val hostname = "localhost"
  val knownAddress = new InetSocketAddress(hostname, 6789)

  object TestSettings extends SettingsMock {
    override lazy val dataDirOpt: Option[String] = None
    override lazy val knownPeers: Seq[InetSocketAddress] = Seq(knownAddress)
    override lazy val nodeNonce: Long = 123456789
  }

  trait App extends ApplicationMock {
    override lazy val settings = TestSettings
  }

  private val app = stub[App]

  import app.basicMessagesSpecsRepo._

  protected override val actorRef = system.actorOf(Props(classOf[PeerManager], app))

  testSafely {

    val peerConnectionHandler = TestProbe("connection-handler")

    def connected(address: InetSocketAddress) = Connected(address, peerConnectionHandler.ref)

    def handshaked(address: InetSocketAddress, noneNonce: Long) =
      Handshaked(address, Handshake("scorex", ApplicationVersion(0, 0, 0), "", noneNonce, None, 0))

    def connect(address: InetSocketAddress, noneNonce: Long): Unit = {
      actorRef ! connected(address)
      actorRef ! handshaked(address, noneNonce)
    }

    def getConnectedPeers =
      Await.result((actorRef ? GetConnectedPeers).mapTo[Seq[(InetSocketAddress, Handshake)]], testDuration)

    def getBlacklistedPeers =
      Await.result((actorRef ? GetBlacklistedPeers).mapTo[Map[InetSocketAddress, PeerInfo]], testDuration)

    val anAddress = new InetSocketAddress(hostname, knownAddress.getPort + 1)

    "peer cycle" - {
      val nonce = 777

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

      "double connect" in {

        actorRef ! CheckPeers

        networkController.expectMsg(NetworkController.ConnectTo(knownAddress))

        connect(knownAddress, nonce)

        getConnectedPeers.size shouldBe 1

        actorRef ! CheckPeers

        networkController.expectNoMsg(testDuration)

        getConnectedPeers.size shouldBe 1

        actorRef ! Disconnected(anAddress)
        getConnectedPeers shouldBe empty
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

      "blacklisting" in {
        // workaround for peer database bug
        actorRef ! AddOrUpdatePeer(anAddress, None, None)

        actorRef ! AddToBlacklist(nonce, new InetSocketAddress(9999))

        peerConnectionHandler.expectMsg(CloseConnection)

        getBlacklistedPeers.size shouldBe 1
      }
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
      assert(h.applicationName == "scorex")
    }

    "get random peers" in {
      actorRef ! AddOrUpdatePeer(new InetSocketAddress(100), Some(TestSettings.nodeNonce), None)
      actorRef ! AddOrUpdatePeer(new InetSocketAddress(101), Some(1), None)

      val peers = Await.result((actorRef ? GetRandomPeers(3)).mapTo[Seq[InetSocketAddress]], testDuration)

      peers.size shouldBe 2
      peers.map(_.getPort) should not contain 100
    }

    "blacklist nonconnected peer" in {
      getBlacklistedPeers shouldBe empty

      actorRef ! AddToBlacklist(-111, knownAddress)

      getBlacklistedPeers.size shouldBe 1
    }
  }
}
