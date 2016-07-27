package scorex.lagonaki.integration

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import scorex.block.Block
import scorex.block.Block._
import scorex.lagonaki.ActorTestingCommons
import scorex.lagonaki.mocks.{ApplicationMock, BlockMock}
import scorex.network.NetworkController.{DataFromPeer, RegisterMessagesHandler, SendToNetwork}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.{BlockchainSynchronizer, ConnectedPeer, PeerConnectionHandler, SendToChosen}
import scorex.settings.SettingsMock
import scorex.transaction.History

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.{implicitConversions, postfixOps}
import scala.util.Random

class BlockchainSynchronizerSpecification extends ActorTestingCommons {

  import BlockchainSynchronizer._
  import scorex.network.Coordinator._

  private def mockHistory(last: Int): History = {
    val history = mock[History]
    (history.contains(_: BlockId)) expects * onCall { id: BlockId => id(0) <= last } anyNumberOfTimes()
    history
  }

  private def blockIds(i: Int*): BlockIds = i.map(toBlockId)
  private implicit def toBlockId(i: Int): BlockId = Array(i.toByte)

  private def mockBlock[Id](id: Id)(implicit conv: Id => BlockId): Block =
    new BlockMock(Seq.empty) {
      override val uniqueId: BlockId = id
    }

  private val h = mockHistory(10)

  private val networkController = TestProbe("NetworkController")
  private val coordinator = TestProbe("Coordinator")

  private val peerHandler = TestProbe("PeerHandler")
  private val peer = ConnectedPeer(new InetSocketAddress(9977), peerHandler.ref)

  object TestSettings extends SettingsMock {
    override lazy val historySynchronizerTimeout: FiniteDuration = 1 seconds
    override lazy val forkMaxLength: Int = 10
  }

  private def synchronizerTimeout = TestSettings.historySynchronizerTimeout
  private def withinReasonableTimeInterval = synchronizerTimeout / 10

  trait A extends ApplicationMock {
    override lazy val settings = TestSettings
    override lazy val networkController: ActorRef = BlockchainSynchronizerSpecification.this.networkController.ref
    override lazy val coordinator: ActorRef = BlockchainSynchronizerSpecification.this.coordinator.ref
    override lazy val history: History = h
  }

  val app = stub[A]

  import app.basicMessagesSpecsRepo._

  private def validateStatus(status: Status): Unit = {
    blockChainSynchronizerRef ! GetStatus
    expectMsg(status)
  }

  private def assertLatestBlockFromNonSyncPeer(): Unit = {
    val testPeerHandler = TestProbe()
    val peer = ConnectedPeer(new InetSocketAddress(9977), testPeerHandler.ref)

    val block = new BlockMock(Seq.empty)
    blockChainSynchronizerRef ! DataFromPeer(BlockMessageSpec.messageCode, block, peer)
    coordinator.expectMsg(AddBlock(block, Some(peer)))
  }

  trait TestDataExtraction[T] {
    def extract(actual: T) : Any
  }

  implicit object GetSignaturesSpecExtraction extends TestDataExtraction[BlockIds] {
    override def extract(blockIds: BlockIds): Seq[Int] = blockIds.map(GetBlockSpecExtraction.extract)
  }

  implicit object GetBlockSpecExtraction extends TestDataExtraction[BlockId] {
    override def extract(blockId: BlockId): Int = blockId(0)
  }

  private def expectNetworkMessage[Content : TestDataExtraction](expectedSpec: MessageSpec[Content], expectedData: Any): Unit =
    networkController.expectMsgPF() {
      case SendToNetwork(Message(spec, Right(data: Content@unchecked), None), SendToChosen(peers)) =>
        peers should contain (peer)
        spec shouldEqual expectedSpec
        implicitly[TestDataExtraction[Content]].extract(data) shouldEqual expectedData
    }

  private def expectedGetSignaturesSpec(blockIds: Int*): Unit = expectNetworkMessage(GetSignaturesSpec, blockIds.toSeq)

  private def sendData[C](spec: MessageSpec[C], data: C, fromPeer: ConnectedPeer = peer): Unit =
    blockChainSynchronizerRef ! DataFromPeer(spec.messageCode, data, fromPeer)

  private def sendBlock(block: Block): Unit = sendData(BlockMessageSpec, block)
  private def sendSignatures(blockIds: BlockId*): Unit = sendData(SignaturesSpec, blockIds.toSeq)

  val blockChainSynchronizerRef = system.actorOf(Props(classOf[BlockchainSynchronizer], app))

  testSafely {

    networkController.expectMsgType[RegisterMessagesHandler]

    validateStatus(Idle)
    assertLatestBlockFromNonSyncPeer()

    val t = System.currentTimeMillis()
    def adjustedSynchronizerTimeout = synchronizerTimeout.toMillis - (System.currentTimeMillis() - t) millis

    blockChainSynchronizerRef ! GetExtension(blockIds(10, 9), Map(peer -> 0))
    expectedGetSignaturesSpec(10, 9)

    "go to GettingExtension" - {

      validateStatus(GettingExtension)

      assertLatestBlockFromNonSyncPeer()

      sendSignatures(9, 10, 11, 12, 13)

      expectedGetSignaturesSpec(13, 12)

      "sending same signatures twice should not lead to blacklisting" in {
        sendSignatures(9, 10, 11, 12, 13)
        peerHandler.expectNoMsg(withinReasonableTimeInterval)
      }

      "go to GettingExtensionTail" - {

        validateStatus(GettingExtensionTail)

        val validBlockIds = blockIds(13, 14, 15)

        "extension tail from another peer(s)" - {
          val anotherPeer = TestProbe("another")
          sendData(SignaturesSpec, validBlockIds, ConnectedPeer(new InetSocketAddress(8967), anotherPeer.ref))

          "should not lead to blackkisting of the peers" in {
            peerHandler.expectNoMsg(withinReasonableTimeInterval)
          }

          "should not lead to any futher actions" in {
            peerHandler.expectNoMsg(withinReasonableTimeInterval)
          }
        }

        "follow ledger download scenario" - {

          sendSignatures(validBlockIds: _*)

          expectedGetSignaturesSpec(15, 14)

          sendSignatures(14, 15)

          val finalBlockIdInterval = 11 to 15

          finalBlockIdInterval foreach (expectNetworkMessage(GetBlockSpec, _))

          validateStatus(GettingBlocks)

          "blocks loading" - {

            assertLatestBlockFromNonSyncPeer()

            "same block twice should not reset timeout" in {
              val aBlockId = finalBlockIdInterval.head

              sendBlock(mockBlock(aBlockId))

              Thread sleep (adjustedSynchronizerTimeout.toMillis * 0.99).toLong

              sendBlock(mockBlock(aBlockId))

              peerHandler.expectMsg(withinReasonableTimeInterval, PeerConnectionHandler.Blacklist)
            }

            "happy path" in {
              Random.shuffle(finalBlockIdInterval) foreach { id => sendBlock(mockBlock(id)) }

              coordinator.expectMsgPF(hint = s"${finalBlockIdInterval.size} fork blocks") {
                case ApplyFork(blocks, connectedPeer) =>
                  connectedPeer == peer
                  blocks.map(id => InnerId(id.uniqueId)) == blockIds(finalBlockIdInterval: _*).map(InnerId)
              }

              coordinator.expectMsg(SyncFinished(success = true))
              validateStatus(Idle)

              peerHandler.expectNoMsg((synchronizerTimeout.toMillis * 1.1).toLong millis)
            }
          }

          "react on GetExtension in the Idle state only" in {
            blockChainSynchronizerRef ! GetExtension(blockIds(10), Map(peer -> 0))

            validateStatus(GettingBlocks)
          }
        }
      }
    }

    "become idle on timeout" in {
      coordinator.expectNoMsg(adjustedSynchronizerTimeout)

      peerHandler.expectMsg(PeerConnectionHandler.Blacklist)
      coordinator.expectMsg(SyncFinished(success = false))

      validateStatus(Idle)
    }
  }
}
