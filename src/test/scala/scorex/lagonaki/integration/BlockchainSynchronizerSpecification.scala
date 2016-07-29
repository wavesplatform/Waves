package scorex.lagonaki.integration

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import scorex.app.Application
import scorex.block.Block
import scorex.block.Block._
import scorex.consensus.ConsensusModule
import scorex.lagonaki.ActorTestingCommons
import scorex.lagonaki.mocks.{ApplicationMock, BlockMock}
import scorex.network.NetworkController.{DataFromPeer, RegisterMessagesHandler}
import scorex.network.message.BasicMessagesRepo
import scorex.network.{BlockchainSynchronizer, ConnectedPeer, PeerConnectionHandler}
import scorex.settings.SettingsMock
import scorex.transaction.{History, TransactionModule}

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

  private val h = mockHistory(10)

  private val coordinator = TestProbe("Coordinator")

  object TestSettings extends SettingsMock {
    override lazy val historySynchronizerTimeout: FiniteDuration = 1 seconds
    override lazy val forkMaxLength: Int = 10
    override lazy val operationAttempts: Int = 1
  }

  trait A extends Application {
    implicit val txModule = mock[TransactionModule[Int]]
    implicit val consModule = mock[ConsensusModule[Int]]
    override val basicMessagesSpecsRepo: BasicMessagesRepo = new BasicMessagesRepo()
    override lazy val settings = TestSettings
    override lazy val networkController: ActorRef = networkControllerMock
    override lazy val coordinator: ActorRef = BlockchainSynchronizerSpecification.this.coordinator.ref
    override lazy val history: History = h
  }

  val app = stub[A]

  import app.basicMessagesSpecsRepo._

  private def validateStatus(status: Status): Unit = {
    actorRef ! GetStatus
    expectMsg(status)
  }

  private def assertLatestBlockFromNonSyncPeer(): Unit = {
    val testPeerHandler = TestProbe()
    val peer = ConnectedPeer(new InetSocketAddress(peerId + 1), testPeerHandler.ref)

    val block = new BlockMock(Seq.empty)
    actorRef ! DataFromPeer(BlockMessageSpec.messageCode, block, peer)
    coordinator.expectMsg(AddBlock(block, Some(peer)))
  }

  private def expectedGetSignaturesSpec(blockIds: Int*): Unit = expectNetworkMessage(GetSignaturesSpec, blockIds.toSeq)
  private def sendBlock(block: Block): Unit = dataFromNetwork(BlockMessageSpec, block)
  private def sendSignatures(blockIds: BlockId*): Unit = dataFromNetwork(SignaturesSpec, blockIds.toSeq)

  protected override val actorRef = system.actorOf(Props(classOf[BlockchainSynchronizer], app))

  testSafely {

    validateStatus(Idle)
    assertLatestBlockFromNonSyncPeer()

    val t = System.currentTimeMillis()

    def adjustedTimeout(correction: Float): Long = {
      val withElapsedTime = TestSettings.historySynchronizerTimeout.toMillis - (System.currentTimeMillis() - t)
      withElapsedTime * correction toLong
    }

    def aBitLessThanTimeout = adjustedTimeout(0.9f) millis
    def aBitLongerThanTimeout = adjustedTimeout(1.1f) millis
    def withinReasonableTimeInterval = (TestSettings.historySynchronizerTimeout.toMillis / 2) millis

    actorRef ! GetExtension(blockIds(10, 9), Map(peer -> 0)) // ids come in reverse order
    expectedGetSignaturesSpec(10, 9)

    validateStatus(GettingExtension)

    "become idle on timeout in GettingExtension" in {
      coordinator.expectNoMsg(aBitLessThanTimeout)

      coordinator.expectMsg(SyncFinished(success = false))

      validateStatus(Idle)
    }

    "go to GettingExtension" - {

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
          dataFromNetwork(SignaturesSpec, validBlockIds, ConnectedPeer(new InetSocketAddress(peerId + 2), anotherPeer.ref))

          "should not lead to blackkisting of the peers" in {
            peerHandler.expectNoMsg(withinReasonableTimeInterval)
          }

          "should not lead to any futher actions" in {
            peerHandler.expectNoMsg(withinReasonableTimeInterval)
          }
        }

        "blacklist on timeout in states following GettingExtension" in {
          coordinator.expectNoMsg(aBitLessThanTimeout)

          peerHandler.expectMsg(PeerConnectionHandler.Blacklist)
          coordinator.expectMsg(SyncFinished(success = false))

          validateStatus(Idle)
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

              Thread sleep aBitLessThanTimeout.toMillis

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

              peerHandler.expectNoMsg(aBitLongerThanTimeout)
            }
          }

          "react on GetExtension in the Idle state only" in {
            actorRef ! GetExtension(blockIds(10), Map(peer -> 0))

            validateStatus(GettingBlocks)
          }
        }
      }
    }
  }
}
