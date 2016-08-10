package scorex.lagonaki.integration

import java.net.InetSocketAddress

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.h2.mvstore.MVStore
import scorex.block.Block
import scorex.block.Block._
import scorex.lagonaki.ActorTestingCommons
import scorex.lagonaki.mocks.BlockMock
import scorex.network.NetworkController.DataFromPeer
import scorex.network.{BlockchainSynchronizer, ConnectedPeer, PeerConnectionHandler}
import scorex.settings.SettingsMock
import scorex.transaction.state.database.blockchain.StoredBlockSeq
import scorex.transaction.{BlockStorage, History}

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.{implicitConversions, postfixOps}

class BlockchainSynchronizerSpecification extends ActorTestingCommons {

  import BlockchainSynchronizer._
  import scorex.network.Coordinator._

  private def mockHistory(last: Int): History = {
    val history = mock[History]
    (history.contains(_: BlockId)) expects * onCall { id: BlockId => id(0) <= last } anyNumberOfTimes()
    history
  }

  private val lastHistoryBlockId = 10
  private val testHistory = mockHistory(lastHistoryBlockId)

  private val testCoordinator = TestProbe("Coordinator")

  private val entireForkLoad = mockFunction[Boolean]
  private def setloadEntireForkChunk(value: Boolean) = entireForkLoad expects() returns value anyNumberOfTimes

  object TestSettings extends SettingsMock {
    override lazy val historySynchronizerTimeout: FiniteDuration = 1 second
    override lazy val forkMaxLength: Int = lastHistoryBlockId
    override lazy val retriesBeforeBlacklisted: Int = 1
    override lazy val operationRetries: Int = retriesBeforeBlacklisted + 13930975
    override lazy val pinToInitialPeer: Boolean = true
    override lazy val minForkChunkSize: Int = 1
    override lazy val loadEntireForkChunk: Boolean = entireForkLoad()
  }

  private val blockScore = BigInt(100)

  private trait App extends ApplicationMock {

    class StoredBlockSeqMock extends StoredBlockSeq(new MVStore.Builder().open()) {
      override protected[this] def toBytes(block: Block): Array[Byte] = block.uniqueId
      override protected[this] def fromBytes(bytes: Array[Byte]): Option[Block] = Some(mockBlock(bytes))
    }

    private val testBlockStorage = mock[BlockStorage]
    testBlockStorage.blockSeq _ expects() returns new StoredBlockSeqMock anyNumberOfTimes

    consensusModule.blockScore _ expects * returns blockScore anyNumberOfTimes()

    override lazy val settings = TestSettings
    override lazy val coordinator: ActorRef = testCoordinator.ref
    override lazy val history: History = testHistory
    override val blockStorage: BlockStorage = testBlockStorage
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
    testCoordinator.expectMsg(AddBlock(block, Some(peer)))
  }

  private def assertThatPeerGotBlacklisted(): Unit = {
    within(reasonableTimeInterval) {
      peerHandler.expectMsg(PeerConnectionHandler.Blacklist)
      testCoordinator.expectMsg(SyncFinished.unsuccessfully)
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
      testCoordinator.expectNoMsg(aBitLessThanTimeout)

      testCoordinator.expectMsg(SyncFinished.unsuccessfully)

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

          "should not lead to blacklisting of the peers" in {
            peerHandler.expectNoMsg(reasonableTimeInterval)
          }

          "should not lead to any futher actions" in {
            peerHandler.expectNoMsg(reasonableTimeInterval)
          }
        }

        "get idle on timeout in states following GettingExtension" in {
          testCoordinator.expectNoMsg(aBitLessThanTimeout)

          testCoordinator.expectMsg(reasonableTimeInterval, SyncFinished.unsuccessfully)

          validateStatus(Idle)
        }

        "follow ledger download scenario" - {

          sendSignatures(validBlockIds: _*)

          expectedGetSignaturesSpec(15, 14)

          val initialScore = BigInt(1000)

          testHistory.scoreOf _ expects * onCall {
            blockId: BlockId =>
              assert(BlockIdExtraction.extract(blockId) == lastHistoryBlockId, s"defined only for block id $lastHistoryBlockId")
              initialScore
          } noMoreThanOnce()

          sendSignatures(14, 15)

          val finalBlockIdInterval = 11 to 15

          validateStatus(GettingBlocks)

          "react on GetExtension in the Idle state only" in {
            actorRef ! GetExtension(blockIds(lastHistoryBlockId), Map(peer -> 10000))

            validateStatus(GettingBlocks)
          }

          "blocks loading" - {

            assertLatestBlockFromNonSyncPeer()

            val numberOfBlocks = finalBlockIdInterval.size

            def setHistoryScoreExpectations(delta: BigInt): Unit =
              testHistory.score _ expects() returns (initialScore + (numberOfBlocks * blockScore) + delta) repeat (0 to numberOfBlocks)

            def sendBlocks(): Unit = finalBlockIdInterval foreach { id =>
              expectNetworkMessage(GetBlockSpec, id); sendBlock(mockBlock(id)) }

            def assertThatBlocksLoaded(): Unit = {
              testCoordinator.expectMsgPF(hint = s"$numberOfBlocks fork blocks") {
                case SyncFinished(true, Some((lastCommonBlockId, blockIterator, Some(connectedPeer)))) =>
                  connectedPeer shouldBe peer
                  BlockIdExtraction.extract(lastCommonBlockId) shouldBe lastHistoryBlockId

                  val forkStorageBlockIds = blockIterator.map(id => InnerId(id.uniqueId)).toSeq
                  forkStorageBlockIds shouldBe blockIds(finalBlockIdInterval: _*).map(InnerId)
              }

              validateStatus(Idle)
              peerHandler.expectNoMsg(aBitLongerThanTimeout)
            }

            "entire fork loading" - {

              setloadEntireForkChunk(true)

              "fork has two blocks better score" in {
                setHistoryScoreExpectations(-(blockScore*2 + 1))

                sendBlocks()

                assertThatBlocksLoaded()
              }
            }

            "partial fork laoding" - {

              setloadEntireForkChunk(false)

              "fork has lower score" in {
                setHistoryScoreExpectations(1)

                sendBlocks()

                assertThatPeerGotBlacklisted()

                validateStatus(Idle)
              }

              "fork has better score" - {

                setHistoryScoreExpectations(-1)

                "same block twice should not reset timeout" in {
                  val firstSubsequentBlockId = finalBlockIdInterval.head

                  sendBlock(mockBlock(firstSubsequentBlockId))

                  Thread sleep aBitLessThanTimeout.toMillis

                  sendBlock(mockBlock(firstSubsequentBlockId))

                  testCoordinator.expectMsg(reasonableTimeInterval, SyncFinished.unsuccessfully)
                }

                "happy path" in {
                  sendBlocks()
                  assertThatBlocksLoaded()
                }
              }
            }
          }
        }
      }
    }

    "a (sub)sequience of block ids to download" - {

      implicit def toInnerIds(i: Seq[Int]): InnerIds = i.map(toInnerId)
      implicit def toInnerId(i: Int): InnerId = InnerId(Array(i.toByte))

      def historyContaining(blockIds: Int*): History = {
        val history = mock[History]
        (history.contains(_: BlockId)) expects * onCall { id: BlockId => blockIds.contains(id.head.toInt) } anyNumberOfTimes()
        history
      }

      def test(blockIds: InnerIds, h: History, expectedLastCommon: InnerId, expected: Seq[Int]): Unit = {
        val Some((commonId, tail)) = BlockchainSynchronizer.blockIdsToStartDownload(blockIds, h)
        commonId shouldBe expectedLastCommon
        tail should contain theSameElementsInOrderAs toInnerIds(expected)
      }

      "a sample sequience" in {
        test(Seq(1, 2, 3, 4), historyContaining(1, 2), 2, Seq(3, 4))
      }

      "all blocks are in history" in {
        test(Seq(1, 2, 3, 4), historyContaining(1, 2, 3, 4), 4, Seq())
      }

      "suspicious block id" in {
        test(Seq(1, 2, 3, 4), historyContaining(1, 3), 1, Seq(2, 3, 4))
      }

      "first block(s) are not in history" in {
        blockIdsToStartDownload(Seq(10000, 2, 3, 4), historyContaining(1, 2, 3)) shouldEqual None
      }
    }
  }
}
