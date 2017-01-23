package scorex.network

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.consensus.mining.BlockGeneratorController.StartGeneration
import scorex.network.BlockchainSynchronizer.{GetExtension, GetSyncStatus}
import scorex.network.ScoreObserver.CurrentScore
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.settings.SettingsMock
import scorex.transaction.History

import scala.concurrent.Await
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps

class CoordinatorSpecification extends ActorTestingCommons {

  import scorex.network.Coordinator._

  val testblockGenerator = TestProbe("blockGenerator")
  val testBlockchainSynchronizer = TestProbe("BlockChainSynchronizer")
  val testPeerManager = TestProbe("PeerManager")

  object TestSettings extends SettingsMock {
    override lazy val quorum: Int = 1
    override lazy val scoreBroadcastDelay: FiniteDuration = 1000.seconds
  }

  val testHistory = stub[History]

  trait App extends ApplicationMock {
    override lazy val settings = TestSettings
    override lazy val blockGenerator: ActorRef = testblockGenerator.ref
    override lazy val blockchainSynchronizer: ActorRef = testBlockchainSynchronizer.ref
    override lazy val peerManager: ActorRef = testPeerManager.ref
    override lazy val history: History = testHistory
  }

  override protected val actorRef = system.actorOf(Props(classOf[Coordinator], stub[App]))

  private def getStatus = Await.result((actorRef ? GetCoordinatorStatus).mapTo[CoordinatorStatus], testDuration)

  private def withSyncStatus(syncStatus: BlockchainSynchronizer.Status): String = {
    val future = actorRef ? GetStatus

    testBlockchainSynchronizer.expectMsg(GetSyncStatus)
    testBlockchainSynchronizer.reply(syncStatus)

    Await.result(future.mapTo[String], testDuration)
  }

  testSafely {
    "status" in {
      withSyncStatus(BlockchainSynchronizer.Idle) shouldBe CIdle.name

      val syncStatus = BlockchainSynchronizer.GettingBlocks
      val status = withSyncStatus(syncStatus)
      status should include (CIdle.name)
      status should include (syncStatus.name)
    }

    "starts in synced state with blocks generation" in {
      testblockGenerator.expectMsg(StartGeneration)
    }

    "sync" - {
      getStatus shouldEqual CIdle

      val connectedPeer = stub[ConnectedPeer]
      val score = BigInt(1000)

      testHistory.score _ when() returns(score - 1)

      actorRef ! CurrentScore(Seq((connectedPeer, score)))

      testPeerManager.expectMsg(GetConnectedPeersTyped)

      "with connected peers sync should begin" in {
        testPeerManager.reply(ConnectedPeers(Set(connectedPeer)))

        testBlockchainSynchronizer.expectMsg(GetExtension(Map(connectedPeer -> score)))

        val expectedStatus = CSyncing
        getStatus shouldEqual expectedStatus

        val syncStatus = BlockchainSynchronizer.GettingExtension
        val status = withSyncStatus(syncStatus)
        status should include (expectedStatus.name)
        status should include (syncStatus.name)
      }

      "no connected peers => no sync" in {
        testPeerManager.reply(ConnectedPeers(Set.empty))

        testBlockchainSynchronizer.expectNoMsg(testDuration)
        getStatus shouldEqual CIdle
      }

      "connected peers don't match score peers => no sync" in {
        testPeerManager.reply(ConnectedPeers(Set(stub[ConnectedPeer])))

        testBlockchainSynchronizer.expectNoMsg(testDuration)
        getStatus shouldEqual CIdle
      }
    }
  }
}
