package scorex.lagonaki.integration

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import scorex.block.Block
import scorex.block.Block._
import scorex.lagonaki.ActorTestingCommons
import scorex.lagonaki.mocks.BlockMock
import scorex.network.NetworkController.DataFromPeer
import scorex.network.{BlockchainSynchronizer, ConnectedPeer, PeerConnectionHandler}
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

  private val lastHistoryBlockId = 10
  private val h = mockHistory(lastHistoryBlockId)

  private val coordinator = TestProbe("Coordinator")

  object TestSettings extends SettingsMock {
    override lazy val historySynchronizerTimeout: FiniteDuration = 1 seconds
    override lazy val forkMaxLength: Int = lastHistoryBlockId
    override lazy val retriesBeforeBlacklisted: Int = 1
    override lazy val operationRetries: Int = retriesBeforeBlacklisted + 13930975
    override lazy val pinToInitialPeer: Boolean = true
  }

  private trait App extends ApplicationMock {
    override lazy val settings = TestSettings
    override lazy val coordinator: ActorRef = BlockchainSynchronizerSpecification.this.coordinator.ref
    override lazy val history: History = h
  }

  private val app = stub[App]

  import app.basicMessagesSpecsRepo._

  private def reasonableTimeInterval = (TestSettings.historySynchronizerTimeout.toMillis / 2) millis

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

  private def assertThatPeerGotBlacklisted(): Unit = {
    within(reasonableTimeInterval) {
      peerHandler.expectMsg(PeerConnectionHandler.Blacklist)
      coordinator.expectMsg(SyncFinished.unsuccessfully)
    }
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

    actorRef ! GetExtension(blockIds(lastHistoryBlockId, 9), Map(peer -> 0)) // ids come in reverse order
    expectedGetSignaturesSpec(lastHistoryBlockId, 9)

    validateStatus(GettingExtension)

    "at least one block id in response must be among requested ones" in {
      val notInTheHistoryBlockId = lastHistoryBlockId + 1
      val notRequestedBlockFromHistoryBeginning = 1

      sendSignatures(notRequestedBlockFromHistoryBeginning, notInTheHistoryBlockId)

      assertThatPeerGotBlacklisted()
    }

    "become idle on timeout in GettingExtension" in {
      coordinator.expectNoMsg(aBitLessThanTimeout)

      coordinator.expectMsg(SyncFinished.unsuccessfully)

      validateStatus(Idle)
    }

    "go to GettingExtension" - {

      assertLatestBlockFromNonSyncPeer()

      sendSignatures(9, lastHistoryBlockId, 11, 12, 13)

      expectedGetSignaturesSpec(13, 12)

      "sending same signatures twice should not lead to blacklisting" in {
        sendSignatures(9, lastHistoryBlockId, 11, 12, 13)
        peerHandler.expectNoMsg(reasonableTimeInterval)
      }

      "go to GettingExtensionTail" - {

        validateStatus(GettingExtensionTail)

        val validBlockIds = blockIds(13, 14, 15)

        "extension tail from another peer(s)" - {
          val anotherPeer = TestProbe("another")
          dataFromNetwork(SignaturesSpec, validBlockIds, ConnectedPeer(new InetSocketAddress(peerId + 2), anotherPeer.ref))

          "should not lead to blackkisting of the peers" in {
            peerHandler.expectNoMsg(reasonableTimeInterval)
          }

          "should not lead to any futher actions" in {
            peerHandler.expectNoMsg(reasonableTimeInterval)
          }
        }

        "blacklist on timeout in states following GettingExtension" in {
          coordinator.expectNoMsg(aBitLessThanTimeout)

          assertThatPeerGotBlacklisted()

          validateStatus(Idle)
        }

        "follow ledger download scenario" - {

          sendSignatures(validBlockIds: _*)

          expectedGetSignaturesSpec(15, 14)

          sendSignatures(14, 15)

          val finalBlockIdInterval = 11 to 15

          finalBlockIdInterval foreach (expectNetworkMessage(GetBlockSpec, _))

          validateStatus(GettingBlocks)

          "react on GetExtension in the Idle state only" in {
            actorRef ! GetExtension(blockIds(lastHistoryBlockId), Map(peer -> 0))

            validateStatus(GettingBlocks)
          }

          "blocks loading" - {

            assertLatestBlockFromNonSyncPeer()

            "same block twice should not reset timeout" in {
              val aBlockId = finalBlockIdInterval.head

              sendBlock(mockBlock(aBlockId))

              Thread sleep aBitLessThanTimeout.toMillis

              sendBlock(mockBlock(aBlockId))

              assertThatPeerGotBlacklisted()
            }

            "happy path" in {
              Random.shuffle(finalBlockIdInterval) foreach { id => sendBlock(mockBlock(id)) }

              coordinator.expectMsgPF(hint = s"${finalBlockIdInterval.size} fork blocks") {
                case SyncFinished(true, Some((blocks, Some(connectedPeer), true))) =>
                  connectedPeer == peer
                  blocks.map(id => InnerId(id.uniqueId)) == blockIds(finalBlockIdInterval: _*).map(InnerId)
              }

              validateStatus(Idle)

              peerHandler.expectNoMsg(aBitLongerThanTimeout)
            }
          }
        }
      }
    }
  }
}
