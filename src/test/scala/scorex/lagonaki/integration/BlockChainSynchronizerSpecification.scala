package scorex.lagonaki.integration

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.EllipticCurveImpl
import scorex.lagonaki.mocks.{ApplicationMock, BlockMock}
import scorex.network.NetworkController.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.{BlockChainSynchronizer, ConnectedPeer, SendToChosen}
import scorex.settings.SettingsMock
import scorex.transaction.History

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps

class BlockChainSynchronizerSpecification
  extends TestKit(ActorSystem("BlockChainSynchronizerSpecification"))
    with ImplicitSender
    with WordSpecLike
    with BeforeAndAfter
    with Matchers
    with OneInstancePerTest
    with MockFactory {

  after {
    shutdown()
  }

  object TestSettings extends SettingsMock {
    override lazy val historySynchronizerTimeout: FiniteDuration = 1 seconds
  }

  val testNetworkController = TestProbe("NetworkController")
  val testCoordinator = TestProbe("Coordinator")

  private def blockId(fillBy: Byte) = Array.fill(EllipticCurveImpl.SignatureLength)(fillBy)

  val parentId = blockId(1)
  val id1 = blockId(11)
  val id2 = blockId(12)

  val h = stub[History]

  (h.contains(_: BlockId)).when(parentId).returns(true)
  (h.contains(_: BlockId)).when(id1).returns(false)
  (h.contains(_: BlockId)).when(id2).returns(false)

  trait A extends ApplicationMock {
    override implicit lazy val settings = TestSettings
    override lazy val networkController: ActorRef = testNetworkController.ref
    override lazy val coordinator: ActorRef = testCoordinator.ref
    override lazy val history: History = h
  }

  val app = stub[A]

  import BlockChainSynchronizer._
  import app.basicMessagesSpecsRepo._
  import scorex.network.Coordinator._

  val blockChainSynchronizerRef =
    system.actorOf(Props(classOf[BlockChainSynchronizer], app))

  "BlockChainSynchronizer" must {

    def validateStatus(status: Status): Unit = {
      blockChainSynchronizerRef ! GetStatus
      expectMsg(status)
    }

    def assertLatestBlockFromPeerForwarding(peer: ConnectedPeer): Unit = {
      val block = new BlockMock(Seq.empty)
      blockChainSynchronizerRef ! DataFromPeer(BlockMessageSpec.messageCode, block, peer)
      testCoordinator.expectMsg(AddBlock(block, Some(peer)))
    }

    val peer = ConnectedPeer(new InetSocketAddress(9977), null)

    def networkMessage[Content](spec: MessageSpec[Content], data: Content) = {
      SendToNetwork(Message[Content](spec, Right(data), None), SendToChosen(Seq(peer)))
    }

    testNetworkController.expectMsgType[RegisterMessagesHandler]

    validateStatus(Idle)
    assertLatestBlockFromPeerForwarding(peer)

    val t0 = System.currentTimeMillis()

    blockChainSynchronizerRef ! GetExtension(Seq(id1), Seq(peer))

    testNetworkController.expectMsg(networkMessage(GetSignaturesSpec, Seq(id1)))

    validateStatus(GettingExtension)

    "become idle on timeout" in {
      testCoordinator.expectNoMsg(TestSettings.historySynchronizerTimeout.toMillis - (System.currentTimeMillis() - t0) millis)
      testCoordinator.expectMsg(SyncFinished(success = false))

      validateStatus(Idle)
    }

    "follow ledger download scenario" in {

      def returnBlock(id: BlockId): Block = {
        val block =
          new BlockMock(Seq.empty) {
            override val uniqueId: BlockId = id
          }

        blockChainSynchronizerRef ! DataFromPeer(BlockMessageSpec.messageCode, block, peer)

        block
      }

      assertLatestBlockFromPeerForwarding(peer)

      blockChainSynchronizerRef ! DataFromPeer(SignaturesSpec.messageCode, Seq(parentId, id1, id2), peer)

      testNetworkController.expectMsg(networkMessage(GetBlockSpec, id1))
      testNetworkController.expectMsg(networkMessage(GetBlockSpec, id2))

      validateStatus(GettingBlocks)

      assertLatestBlockFromPeerForwarding(peer)

      testCoordinator.expectMsg(ApplyFork(Seq(returnBlock(id2), returnBlock(id1)).reverse, peer))
      testCoordinator.expectMsg(SyncFinished(success = true))

      validateStatus(Idle)
    }
  }
}
