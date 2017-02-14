package scorex.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props, Scheduler}
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.WavesSettings
import org.scalatest.mockito.MockitoSugar
import scorex.ActorTestingCommons
import scorex.app.{Application, ApplicationVersion}
import scorex.network.NetworkController.DataFromPeer
import scorex.network.PeerSynchronizer.RequestDataFromPeer
import scorex.network.peer.PeerManager

import scala.language.postfixOps

class TestPeerSynchronizer(app: Application) extends PeerSynchronizer(app) {
  override def scheduler: Scheduler = MockitoSugar.mock[Scheduler]
}

class PeerSynchronizerSpecification extends ActorTestingCommons
  with MockitoSugar {

  private val localConfig = ConfigFactory.parseString(
    """
      |waves {
      |  network {
      |    file: none
      |    nonce: 123456789
      |    peers-data-residence-time: 100s
      |    black-list-residence-time: 1000
      |    max-connections: 10
      |  }
      |}
    """.stripMargin).withFallback(baseTestConfig).resolve()

  val wavesSettings = WavesSettings.fromConfig(localConfig)
  val testPeerManager = TestProbe("peerManager")

  trait App extends ApplicationMock {
    override lazy val settings = wavesSettings
    override val applicationName: String = "test"
    override val appVersion: ApplicationVersion = ApplicationVersion(7, 7, 7)
    override val peerManager: ActorRef = testPeerManager.ref
  }

  private val app = stub[App]

  import app.basicMessagesSpecsRepo._

  protected override val actorRef = system.actorOf(Props(classOf[TestPeerSynchronizer], app))

  testSafely {
    "PeerSynchronizer" - {
      "accept DataFromPeer after requesting it" in {
        val peer = stub[ConnectedPeer]

        actorRef ! RequestDataFromPeer
        actorRef ! DataFromPeer(PeersSpec.messageCode, Seq(new InetSocketAddress(1)), peer)
        testPeerManager.expectMsgType[PeerManager.AddPeer]
      }

      "reject DataFromPeer without request it" in {
        val peer = stub[ConnectedPeer]

        actorRef ! DataFromPeer(PeersSpec.messageCode, Seq(new InetSocketAddress(1)), peer)
        testPeerManager.expectNoMsg()

      }
    }
  }
}
