package scorex.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props, Scheduler}
import akka.testkit.TestProbe
import org.scalatest.mock.MockitoSugar
import scorex.ActorTestingCommons
import scorex.app.{Application, ApplicationVersion}
import scorex.network.NetworkController.DataFromPeer
import scorex.network.PeerSynchronizer.RequestDataFromPeer
import scorex.network.peer.PeerManager
import scorex.settings.SettingsMock

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps

class TestPeerSynchronizer(app: Application) extends PeerSynchronizer(app) {
  override def scheduler: Scheduler = MockitoSugar.mock[Scheduler]
}

class PeerSynchronizerSpecification extends ActorTestingCommons
  with MockitoSugar {

  object TestSettings extends SettingsMock {
    override lazy val dataDirOpt: Option[String] = None
    override lazy val nodeNonce: Long = 123456789
    override lazy val maxConnections: Int = 10
    override lazy val peersDataResidenceTime: FiniteDuration = 100.seconds
    override lazy val blacklistResidenceTimeMilliseconds: Long = 1000
  }

  val testPeerManager = TestProbe("peerManager")

  trait App extends ApplicationMock {
    override lazy val settings = new SettingsMock {}
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
