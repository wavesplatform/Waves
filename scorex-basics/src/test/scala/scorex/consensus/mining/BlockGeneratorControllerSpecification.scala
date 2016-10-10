package scorex.consensus.mining

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.consensus.mining.BlockGeneratorController._
import scorex.network.ConnectedPeer
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.settings.SettingsMock

import scala.language.postfixOps

class BlockGeneratorControllerSpecification extends ActorTestingCommons {

  val offlineGen = stubFunction[Boolean]
  def setOfflineGeneration(value: Boolean) = offlineGen when() returns value

  object TestSettings extends SettingsMock {
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
    actorRef ! GetBlockGenerationStatus
    expectMsg(status.name)
  }

  testSafely {

    "initial status is Idle" in {
      assertStatusIs(Idle)
    }

    "when Idle don't check peers number" in {
      assertStatusIs(Idle)
      actorRef ! SelfCheck
      testPeerManager.expectNoMsg(testDuration)
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
          testPeerManager.reply(ConnectedPeers(Set(stub[ConnectedPeer])))
          assertStatusIs(Generating)
        }

        "gen suspended" in {
          testPeerManager.reply(ConnectedPeers(Set.empty))
          assertStatusIs(Suspended)
        }
      }

      "offlineGeneration = true" - {

        setOfflineGeneration(true)

        "gen allowed even if no peers" in {
          testPeerManager.reply(ConnectedPeers(Set.empty))
          assertStatusIs(Generating)
        }
      }
    }
  }
}
