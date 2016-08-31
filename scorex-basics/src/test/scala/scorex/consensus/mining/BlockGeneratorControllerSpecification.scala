package scorex.consensus.mining

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.consensus.mining.BlockGeneratorController._
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.settings.SettingsMock

import scala.language.postfixOps

class BlockGeneratorControllerSpecification extends ActorTestingCommons {

  val offlineGen = stubFunction[Boolean]
  def setOfflineGeneration(value: Boolean) = offlineGen when() returns value

  object TestSettings extends SettingsMock {
    override lazy val miningThreads: Int = 0
    override lazy val quorum: Int = 1
    override lazy val offlineGeneration: Boolean = offlineGen()
  }

  val testPeerManager = TestProbe("PeerManager")

  trait App extends ApplicationMock {
    override lazy val settings = TestSettings
    override val peerManager: ActorRef = testPeerManager.ref
  }

  override protected val actorRef = system.actorOf(Props(classOf[BlockGeneratorController], stub[App]))

  private def assertStatusIs(status: BlockGeneratorController.Status) = {
    actorRef ! GetStatus
    expectMsg(status.name)
  }

  testSafely {

    "initial status is Idle" in {
      assertStatusIs(Idle)
    }

    "StopGeneration command change state to idle from generating" in {
      actorRef ! StartGeneration
      assertStatusIs(Generating)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
    }

    "StopGeneration command don't change state from idle" in {
      actorRef ! StartGeneration
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
    }

    "suspended / resumed" - {
      actorRef ! StartGeneration

      testPeerManager.expectMsg(GetConnectedPeersTyped)

      "offlineGeneration = false" - {

        setOfflineGeneration(false)

        "gen allowed" in {
          testPeerManager.reply(ConnectedPeers(Seq((new InetSocketAddress(687), null))))
          assertStatusIs(Generating)
        }

        "gen suspended" in {
          testPeerManager.reply(ConnectedPeers(Seq.empty))
          assertStatusIs(Suspended)
        }
      }

      "offlineGeneration = true" - {

        setOfflineGeneration(true)

        "gen allowed even if no peers" in {
          testPeerManager.reply(ConnectedPeers(Seq.empty))
          assertStatusIs(Generating)
        }
      }
    }
  }
}