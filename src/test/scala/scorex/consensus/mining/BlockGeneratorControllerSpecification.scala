package scorex.consensus.mining

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import com.wavesplatform.settings.WavesSettings
import scorex.ActorTestingCommons
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController._
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.transaction.History

import scala.concurrent.duration._
import scala.language.postfixOps

class BlockGeneratorControllerSpecification extends ActorTestingCommons {

  val testPeerManager = TestProbe("PeerManager")

  trait App extends ApplicationMock {
    override lazy val settings: WavesSettings = WavesSettings.fromConfig(baseTestConfig)
    override val peerManager: ActorRef = testPeerManager.ref
    @volatile
    var history: History = _

    private[BlockGeneratorControllerSpecification] def setHistory(history: History) = this.history = history
  }

  val stubApp: App = stub[App]

  setDefaultLastBlock()

  private def setDefaultLastBlock(): Unit = setLastBlock(testBlock(2))

  private def setLastBlock(block: Block, desiredHeight: Int = 1): Unit = stubApp.setHistory(historyWithLastBlock(block, desiredHeight))

  private def historyWithLastBlock(block: Block, desiredHeight: Int): History = {
    val stubHistory = stub[History]
    (stubHistory.lastBlock _).when().returns(block).anyNumberOfTimes()
    (stubHistory.height _).when().returns(desiredHeight).anyNumberOfTimes()
    stubHistory
  }

  private class TestBlockGeneratorController(app: App) extends BlockGeneratorController(app) {
    override def preStart(): Unit = {}
  }

  override protected val actorRef = system.actorOf(Props(new TestBlockGeneratorController(stubApp)))

  private def assertStatusIs(status: BlockGeneratorController.Status) = {
    actorRef ! GetBlockGenerationStatus
    expectMsg(status.name)
  }

  testSafely {

    "initial status is Idle" in {
      assertStatusIs(Idle)
    }

    "when Idle don't check peers number" in {
      setLastBlock(testBlock(2, 0), 2)
      assertStatusIs(Idle)
      actorRef ! SelfCheck
      testPeerManager.expectNoMsg(testDuration)
      setDefaultLastBlock()
    }

    "StopGeneration command change state to idle from generating" in {
      actorRef ! StartGeneration
      assertStatusIs(Generating)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
    }

    "StopGeneration command don't change state from idle" in {
      setLastBlock(testBlock(2, 0))
      actorRef ! StartGeneration
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      setDefaultLastBlock()
    }

    "StartGeneration command change state to generation when should generate because of genesis block" in {
      setLastBlock(testBlock(1, System.currentTimeMillis() - 11.minutes.toMillis))
      assertStatusIs(Idle)
      actorRef ! StartGeneration
      assertStatusIs(Generating)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      setDefaultLastBlock()
    }

    "StartGeneration command change state to generation when should generate because of last block time" in {
      setLastBlock(testBlock(5, System.currentTimeMillis() - 9.minutes.toMillis))
      assertStatusIs(Idle)
      actorRef ! StartGeneration
      assertStatusIs(Generating)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      setDefaultLastBlock()
    }

    "StartGeneration command do not change state to generation when should't generate" in {
      setLastBlock(testBlock(2, 0), 2)
      assertStatusIs(Idle)
      actorRef ! StartGeneration
      assertStatusIs(Idle)
      setDefaultLastBlock()
    }

    "suspended / resumed" - {
      actorRef ! StartGeneration

      testPeerManager.expectMsg(GetConnectedPeersTyped)

      "offlineGeneration = true" - {

        "gen allowed even if no peers" in {
          testPeerManager.reply(ConnectedPeers(Set.empty))
          assertStatusIs(Generating)
        }
      }
    }
  }
}
