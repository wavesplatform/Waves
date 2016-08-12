package scorex.network.peer

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.TestActorRef
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import play.api.libs.json.{JsObject, Json}
import scorex.app.{Application, ApplicationVersion}
import scorex.network.{ConnectedPeer, Handshake}
import scorex.network.peer.PeerManager.{Connected, GetConnectedPeers, Handshaked}
import scorex.settings.Settings

import scala.util.Success
import scala.concurrent.duration._
import akka.util.Timeout

import scala.language.postfixOps

class PeerManagerSpecification extends FunSuite with Matchers with MockFactory with BeforeAndAfterAll {

  implicit val actorSystem = ActorSystem("PeerManagerSpecification")

  implicit val duration: Timeout = 20 seconds

  override def afterAll(): Unit = {
    actorSystem.terminate()
  }

  private object MySettings extends Settings {
    override lazy val settingsJSON: JsObject = Json.obj()
    override lazy val dataDirOpt: Option[String] = None
    override val filename: String = ""
    override lazy val knownPeers = Seq.empty[InetSocketAddress]
  }

  trait MyApp extends Application {
    override val settings: Settings = MySettings
  }

  test("PeerManager returns on GetConnectedPeers list of pairs (InetScoketAddress, Handshake)") {
    val app = stub[MyApp]
    val peerManager = TestActorRef(new PeerManager(app))

    val Success(result : List[(InetSocketAddress, Handshake)]) = (peerManager ? GetConnectedPeers).value.get
    assert(result.isEmpty)

    // prepare
    val peerAddress = new InetSocketAddress(InetAddress.getByAddress(Array[Byte](1, 1, 1, 1)), 1234)
    peerManager ! Connected(ConnectedPeer(peerAddress, null))

    val handshake = Handshake("scorex", ApplicationVersion(1, 1, 1), "", 1, None, 0)
    peerManager ! Handshaked(peerAddress, handshake)

    // assert
    val Success(result2 : List[(InetSocketAddress, Handshake)]) = (peerManager ? GetConnectedPeers).value.get
    assert(result2.nonEmpty)
    val peer = result2.head
    assert(peer._1 == peerAddress)
    assert(peer._2.applicationName == "scorex")

    peerManager.stop()
  }
}
