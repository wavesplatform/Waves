package scorex.lagonaki.integration

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import scorex.consensus.mining.BlockGeneratorController.StartGeneration
import scorex.lagonaki.ActorTestingCommons
import scorex.lagonaki.mocks.ApplicationMock
import scorex.network.BlockchainSynchronizer.{GetStatus, GettingBlocks}
import scorex.network.{BlockchainSynchronizer, Coordinator}
import scorex.settings.SettingsMock
import scorex.transaction.History

import scala.concurrent.Await

class CoordinatorSpecification extends ActorTestingCommons {

  val testNetworkController = TestProbe("NetworkController")
  val testblockGenerator = TestProbe("blockGenerator")
  val testBlockChainSynchronizer = TestProbe("BlockChainSynchronizer")

  val h = stub[History]

  (h.score _).when().returns(BigInt(1))

  object TestSettings extends SettingsMock {
    override lazy val forkResolveQuorumSize: Int = 2
    override lazy val maxPeersToBroadcastBlock: Int = 1
  }

  trait A extends ApplicationMock {
    override lazy val settings = TestSettings
    override lazy val networkController: ActorRef = testNetworkController.ref
    override lazy val blockGenerator: ActorRef = testblockGenerator.ref
    override lazy val blockchainSynchronizer: ActorRef = testBlockChainSynchronizer.ref
    override lazy val history: History = h
  }

  val app = stub[A]

  testSafely {

    val coordinator = system.actorOf(Props(classOf[Coordinator], app))

    "returns its status" in {

      val future = coordinator ? GetStatus

      testBlockChainSynchronizer.expectMsg(GetStatus)
      testBlockChainSynchronizer.reply(GettingBlocks)

      Await.result(future.mapTo[BlockchainSynchronizer.Status], testDuration) should be(GettingBlocks)
    }

    "starts in synced state with blocks generation" in {
      testblockGenerator.expectMsg(StartGeneration)
    }
  }
}
