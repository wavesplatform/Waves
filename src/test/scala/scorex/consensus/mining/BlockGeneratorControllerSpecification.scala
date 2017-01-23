package scorex.consensus.mining

import scala.concurrent.duration.FiniteDuration
import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.consensus.mining.BlockGeneratorController._
import scorex.network.ConnectedPeer
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.settings.SettingsMock
import scala.language.postfixOps
import scorex.block.Block
import scorex.transaction.History
import scala.concurrent.duration._

class BlockGeneratorControllerSpecification extends ActorTestingCommons {

  val offlineGen = stubFunction[Boolean]
  def setOfflineGeneration(value: Boolean) = offlineGen when() returns value

  object TestSettings extends SettingsMock {
    override lazy val quorum: Int = 1
    override lazy val offlineGeneration: Boolean = offlineGen()
    override lazy val allowedGenerationTimeFromLastBlockInterval: FiniteDuration = 10 minutes
  }

  val testPeerManager = TestProbe("PeerManager")

  trait App extends ApplicationMock {
    override lazy val settings = TestSettings
    override val peerManager: ActorRef = testPeerManager.ref
    @volatile
    var history: History = _
    private[BlockGeneratorControllerSpecification] def setHistory(history: History) = this.history = history
  }

  val stubApp: App = stub[App]
  setDefaultLastBlock()

  private def setDefaultLastBlock(): Unit = setLastBlock(blockMock(2))
  private def setLastBlock(block: Block): Unit = stubApp.setHistory(historyWithLastBlock(block))
  private def historyWithLastBlock(block: Block): History = {
    val stubHistory = stub[History]
    (stubHistory.lastBlock _).when().returns(block).anyNumberOfTimes()
    (stubHistory.height _).when().returns(block.uniqueId.head).anyNumberOfTimes()
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
      setLastBlock(blockMock(2, 0))
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
      setLastBlock(blockMock(2, 0))
      actorRef ! StartGeneration
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      setDefaultLastBlock()
    }

    "StartGeneration command change state to generation when should generate because of genesis block" in {
      setLastBlock(blockMock(1, System.currentTimeMillis() - 11.minutes.toMillis))
      assertStatusIs(Idle)
      actorRef ! StartGeneration
      assertStatusIs(Generating)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      setDefaultLastBlock()
    }

    "StartGeneration command change state to generation when should generate because of last block time" in {
      setLastBlock(blockMock(5, System.currentTimeMillis() - 9.minutes.toMillis))
      assertStatusIs(Idle)
      actorRef ! StartGeneration
      assertStatusIs(Generating)
      actorRef ! StopGeneration
      assertStatusIs(Idle)
      setDefaultLastBlock()
    }

    "StartGeneration command do not change state to generation when should't generate" in {
      setLastBlock(blockMock(2, 0))
      assertStatusIs(Idle)
      actorRef ! StartGeneration
      assertStatusIs(Idle)
      setDefaultLastBlock()
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
