package scorex.network

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.network.BlockchainSynchronizer.{GetExtension, GetStatus, GettingBlocks}
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
    override lazy val scoreBroadcastDelay: FiniteDuration = 1000 seconds
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

  testSafely {

    "returns its status" in {

      val future = actorRef ? GetStatus

      testBlockchainSynchronizer.expectMsg(GetStatus)
      testBlockchainSynchronizer.reply(GettingBlocks)

      Await.result(future.mapTo[BlockchainSynchronizer.Status], testDuration) should be(GettingBlocks)
    }

    "sync" - {

      getStatus shouldEqual CIdle

      val connectedPeer = stub[ConnectedPeer]
      val score = BigInt(1000)

      testHistory.score _ when() returns(score - 1)

      actorRef ! CurrentScore(Seq((connectedPeer, score)))

      testPeerManager.expectMsg(GetConnectedPeersTyped)

      "with connected peers sync should begin" in {
        testPeerManager.reply(ConnectedPeers(Seq((new InetSocketAddress(687), null))))

        testBlockchainSynchronizer.expectMsg(GetExtension(Map(connectedPeer -> score)))

        getStatus shouldEqual CSyncing
      }
    }
  }
}
