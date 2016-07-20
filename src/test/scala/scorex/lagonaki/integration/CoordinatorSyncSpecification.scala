package scorex.lagonaki.integration

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.testkit.{TestActorRef, TestKit, TestProbe}
import akka.util.Timeout
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import scorex.app.Application
import scorex.network.BlockChainSynchronizer.{GetStatus, GettingBlocks}
import scorex.network.{BlockChainSynchronizer, Coordinator}
import scorex.settings.SettingsMock
import scorex.transaction.History

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Success

class CoordinatorSyncSpecification
  extends TestKit(ActorSystem("CoordinatorSyncSpecification"))
    with WordSpecLike
    with BeforeAndAfterAll
    with Matchers
    with OneInstancePerTest
    with MockFactory {

  private implicit val _ = Timeout(500 milliseconds)

  override def afterAll {
    shutdown()
  }

  val testBlockChainSynchronizer = TestProbe("BlockChainSynchronizer")

  trait A extends Application {
    override implicit lazy val settings = new SettingsMock {}
    override lazy val blockChainSynchronizer: ActorRef = testBlockChainSynchronizer.ref
    override lazy val history: History = stub[History]
  }

  val app = stub[A]

  "Coordinator" must {

    val actorRef = TestActorRef(Props(classOf[Coordinator], app))

    "return its status" in {
      val future = actorRef ? GetStatus
      testBlockChainSynchronizer.expectMsg(GetStatus)
      testBlockChainSynchronizer.reply(GettingBlocks)

      val Success(status: BlockChainSynchronizer.Status) = future.value.get
      status should be(GettingBlocks)
    }

    "start in synced state with blocks generation" in {
      // copied from HistorySynchronizer test
    }
  }
}
