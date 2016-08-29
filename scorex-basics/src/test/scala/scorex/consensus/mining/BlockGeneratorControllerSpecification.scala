package scorex.consensus.mining

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.consensus.mining.BlockGeneratorController._
import scorex.settings.SettingsMock

import scala.language.postfixOps

class BlockGeneratorControllerSpecification extends ActorTestingCommons {

  object TestSettings extends SettingsMock

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
  }
}