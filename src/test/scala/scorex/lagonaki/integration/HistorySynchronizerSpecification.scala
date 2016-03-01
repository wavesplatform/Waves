package scorex.lagonaki.integration


import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpecLike}
import scorex.consensus.mining.BlockGeneratorController._
import scorex.lagonaki.TestingCommons
import scorex.network.ConnectedPeer
import scorex.network.ScoreObserver.ConsideredValue

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class HistorySynchronizerSpecification(_system: ActorSystem)
  extends TestKit(_system)
  with ScalaFutures
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with TestingCommons {

  implicit val timeout = Timeout(5.seconds)

  def this() = this(ActorSystem("HistorySynchronizerSpecification"))

  val probe = new TestProbe(system)

  val peer = new ConnectedPeer(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 1), probe.ref)

  lazy val peers = Seq(peer)

  lazy val application = TestingCommons.application

  val hs = {
    val res = application.historySynchronizer
    res ! Unit
    Thread.sleep(2.seconds.toMillis)
    res
  }

  "HistorySynchronizer actor" must {
    "start in synced state with blocks generation" in {
      val fStatus = (application.blockGenerator ? GetStatus).map(_.toString)
      whenReady(fStatus) { status =>
        status should equal(Generating.name)
      }
    }

    "stop block generation on better network score" in {
      Thread.sleep(1.second.toMillis)

      hs ! ConsideredValue(Some(BigInt(Long.MaxValue)), peers)

      Thread.sleep(1.second.toMillis)

      val fStatus = (application.blockGenerator ? GetStatus).map(_.toString)
      whenReady(fStatus) { status =>
        status should equal(Syncing.name)
      }
    }
  }
}