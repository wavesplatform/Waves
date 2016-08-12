package scorex.lagonaki.integration

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import org.h2.mvstore.MVStore
import scorex.ActorTestingCommons
import scorex.consensus.mining.BlockGeneratorController.StartGeneration
import scorex.network.BlockchainSynchronizer.{GetStatus, GettingBlocks}
import scorex.network.{BlockchainSynchronizer, Coordinator}
import scorex.settings.SettingsMock
import scorex.transaction.History
import scorex.transaction.state.database.blockchain.StoredBlockchain

import scala.concurrent.Await
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps

class CoordinatorSpecification extends ActorTestingCommons {

  val testblockGenerator = TestProbe("blockGenerator")
  val testBlockChainSynchronizer = TestProbe("BlockChainSynchronizer")

  object TestSettings extends SettingsMock {
    override lazy val forkResolveQuorumSize: Int = 2
    override lazy val maxPeersToBroadcastBlock: Int = 1
    override lazy val scoreBroadcastDelay: FiniteDuration = 1000 seconds
  }

  trait App extends ApplicationMock {
    override lazy val settings = TestSettings
    override lazy val blockGenerator: ActorRef = testblockGenerator.ref
    override lazy val blockchainSynchronizer: ActorRef = testBlockChainSynchronizer.ref
    override lazy val history: History = new StoredBlockchain(new MVStore.Builder().open())
  }

  override protected val actorRef = system.actorOf(Props(classOf[Coordinator], stub[App]))

  testSafely {

    "returns its status" in {

      val future = actorRef ? GetStatus

      testBlockChainSynchronizer.expectMsg(GetStatus)
      testBlockChainSynchronizer.reply(GettingBlocks)

      Await.result(future.mapTo[BlockchainSynchronizer.Status], testDuration) should be(GettingBlocks)
    }

    "starts in synced state with blocks generation" in {
      testblockGenerator.expectMsg(StartGeneration)
    }
  }
}
