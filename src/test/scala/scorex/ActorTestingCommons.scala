package scorex

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKitBase, TestProbe}
import akka.util.Timeout
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FreeSpecLike, Matchers}
import scorex.account.PublicKeyAccount
import scorex.app.Application
import scorex.block.Block._
import scorex.block.{Block, LongBlockField, SignerData}
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData}
import scorex.network.NetworkController.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import scorex.network.message.{BasicMessagesRepo, Message, MessageSpec}
import scorex.network.{ConnectedPeer, SendToChosen, SendingStrategy}
import scorex.transaction.{TransactionModule, TransactionsBlockField}
import scorex.transaction.TypedTransaction.SignatureLength

import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}

abstract class ActorTestingCommons extends TestKitBase
  with org.scalatest.path.FreeSpecLike
  with Matchers
  with ImplicitSender
  with PathMockFactory {

  protected implicit val testTimeout = Timeout(2000.milliseconds)
  protected val testDuration = testTimeout.duration

  implicit final lazy val system = ActorSystem(getClass.getSimpleName)

  protected lazy val networkController = TestProbe("NetworkController")
  protected def networkControllerMock = networkController.ref

  networkController.ignoreMsg {
    case RegisterMessagesHandler(_, _) => true
  }

  protected final def testSafely(fun: => Unit): Unit = getClass.getSimpleName testSafely fun

  protected final class ActorTestingStringWrapper(s: String) {
    def testSafely(fun: => Unit): Unit = {
      s - {
        try {
          fun
        } finally {
          try verifyExpectations
          finally shutdown()
        }
      }
    }
  }

  protected final implicit def convertTo(s: String): ActorTestingStringWrapper = new ActorTestingStringWrapper(s)

  protected lazy val peer = stub[ConnectedPeer]

  protected val actorRef: ActorRef

  protected def dataFromNetwork[C](spec: MessageSpec[C], data: C, fromPeer: ConnectedPeer = peer): Unit =
    actorRef ! DataFromPeer(spec.messageCode, data, fromPeer)

  protected def blockIds(ids: Int*): BlockIds = ids.map(toBlockId)
  protected implicit def toBlockIds(ids: Seq[Int]): BlockIds = blockIds(ids:_*)
  protected implicit def toBlockId(i: Int): BlockId = Array(i.toByte)

  protected def testBlock(id: Int, ts: Long = System.currentTimeMillis()) =
    Block(ts,0,1,SignerData(new PublicKeyAccount(Array.fill(32)(0)),Array(id.toByte)),
      NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)), Seq.empty)

  protected trait TestDataExtraction[T] {
    def extract(actual: T) : Any
  }

  protected implicit object BlockIdsExtraction extends TestDataExtraction[BlockIds] {
    override def extract(blockIds: BlockIds): Seq[Int] = blockIds.map(BlockIdExtraction.extract)
  }

  protected implicit object BlockIdExtraction extends TestDataExtraction[BlockId] {
    override def extract(blockId: BlockId): Int = blockId(0)
  }

  protected def expectNetworkMessage[Content : TestDataExtraction](expectedSpec: MessageSpec[Content], expectedData: Any): Unit =
    expectNetworkMessage(expectedSpec, expectedData, { _.asInstanceOf[SendToChosen].chosenPeers.contains(peer) })

  protected def expectNetworkMessage[Content : TestDataExtraction](expectedSpec: MessageSpec[Content],
                                                                   expectedData: Any,
                                                                   strategyAssertion: SendingStrategy => Boolean): Unit =
    networkController.expectMsgPF(hint = expectedData.toString) {
      case SendToNetwork(Message(spec, Right(data: Content@unchecked), None), st) =>
        strategyAssertion(st) shouldBe true
        spec shouldEqual expectedSpec
        implicitly[TestDataExtraction[Content]].extract(data) shouldEqual expectedData
    }

  trait ApplicationMock extends Application {
    implicit val transactionModule = stub[TransactionModule]
    implicit val consensusModule = stub[ConsensusModule]
    final override val basicMessagesSpecsRepo: BasicMessagesRepo = new BasicMessagesRepo()
    final override lazy val networkController: ActorRef = networkControllerMock
  }
}
