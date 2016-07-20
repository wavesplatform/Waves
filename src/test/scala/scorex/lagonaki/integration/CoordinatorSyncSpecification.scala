package scorex.lagonaki.integration

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.Timeout
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import scorex.consensus.mining.BlockGeneratorController.StartGeneration
import scorex.lagonaki.mocks.ApplicationMock
import scorex.network.BlockChainSynchronizer.{GetStatus, GettingBlocks}
import scorex.network.{BlockChainSynchronizer, Coordinator}
import scorex.transaction.History

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success

class CoordinatorSyncSpecification
  extends TestKit(ActorSystem("CoordinatorSyncSpecification"))
    with ImplicitSender
    with WordSpecLike
    with BeforeAndAfter
    with Matchers
    with OneInstancePerTest
    with MockFactory {

  after {
    shutdown()
  }

  val testNetworkController = TestProbe("NetworkController")
  val testblockGenerator = TestProbe("blockGenerator")
  val testBlockChainSynchronizer = TestProbe("BlockChainSynchronizer")

  val h = stub[History]

  (h.score _).when().returns(BigInt(1))

  trait A extends ApplicationMock {
    override lazy val networkController: ActorRef = testNetworkController.ref
    override lazy val blockGenerator: ActorRef = testblockGenerator.ref
    override lazy val blockChainSynchronizer: ActorRef = testBlockChainSynchronizer.ref
    override lazy val history: History = h
  }

  val app = stub[A]

  "Coordinator" must {

    val coordinator = system.actorOf(Props(classOf[Coordinator], app))

    "return its status" in {

      implicit val _ = Timeout(500 milliseconds)

      val future = coordinator ? GetStatus
      testBlockChainSynchronizer.expectMsg(GetStatus)
      testBlockChainSynchronizer.reply(GettingBlocks)

      val Success(status: BlockChainSynchronizer.Status) = future.mapTo[BlockChainSynchronizer.Status].value.get
      status should be(GettingBlocks)
    }

    "start in synced state with blocks generation" in {
      testblockGenerator.expectMsg(StartGeneration)
    }
  }
}
