package scorex.consensus.mining

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import com.wavesplatform.settings.WavesSettings
import scorex.ActorTestingCommons
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController._
import scorex.network.ConnectedPeer
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.transaction.History

import scala.concurrent.duration._
import scala.language.postfixOps

class OfflineDisabledBlockGeneratorControllerSpecification extends ActorTestingCommons {

  val testPeerManager = TestProbe("PeerManager")

  trait App extends ApplicationMock {
    override lazy val settings: WavesSettings = WavesSettings.fromConfig(testConfigOfflineGenerationOff)
    override val peerManager: ActorRef = testPeerManager.ref
    @volatile
    var history: History = _

    private[OfflineDisabledBlockGeneratorControllerSpecification] def setHistory(history: History) = this.history = history
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

    "suspended / resumed" - {
      actorRef ! StartGeneration

      testPeerManager.expectMsg(GetConnectedPeersTyped)

      "offlineGeneration = false" - {

        "gen allowed" in {
          testPeerManager.reply(ConnectedPeers(Set(stub[ConnectedPeer])))
          assertStatusIs(Generating)
        }

        "gen suspended" in {
          testPeerManager.reply(ConnectedPeers(Set.empty))
          assertStatusIs(Suspended)
        }
      }
    }
  }
}
