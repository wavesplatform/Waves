package scorex.network

import akka.actor.{ActorRef, Props}
import akka.testkit.TestProbe
import org.h2.mvstore.MVStore
import scorex.ActorTestingCommons
import scorex.block.Block
import scorex.block.Block._
import scorex.network.NetworkController.DataFromPeer
import scorex.settings.SettingsMock
import scorex.transaction.{BlockStorage, History}

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

  private case object BlacklistAssertion
  private def setBlacklistExpectations(blacklist: Boolean): Unit = {
    (peer.blacklist _).when().onCall {
      _ => if (blacklist) self ! BlacklistAssertion else fail("No blacklisting should be in this case")
    }
  }

  private val lastHistoryBlockId = 10
  private val testHistory = mockHistory(lastHistoryBlockId)

  private val initialScore = BigInt(1000)

  testHistory.scoreOf _ expects * onCall {
    blockId: BlockId =>
      assert(BlockIdExtraction.extract(blockId) == lastHistoryBlockId, s"defined only for block id $lastHistoryBlockId")
      initialScore
  } noMoreThanOnce()

  private val testCoordinator = TestProbe("Coordinator")

  private val entireForkLoad = mockFunction[Boolean]
  private def setloadEntireForkChunk(value: Boolean) = entireForkLoad expects() returns value anyNumberOfTimes

  object TestSettings extends SettingsMock {
    override lazy val historySynchronizerTimeout: FiniteDuration = testDuration * 2
    override lazy val MaxRollback: Int = lastHistoryBlockId - 1
    override lazy val retriesBeforeBlacklisted: Int = 0
    override lazy val operationRetries: Int = retriesBeforeBlacklisted + 13930975
    override lazy val pinToInitialPeer: Boolean = true
    override lazy val loadEntireChain: Boolean = entireForkLoad()
  }

  private val blockScore = BigInt(100)

  private trait App extends ApplicationMock {

    private val testBlockStorage = mock[BlockStorage]

    consensusModule.blockScore _ when * returns blockScore

    override lazy val settings = TestSettings
    override lazy val coordinator: ActorRef = testCoordinator.ref
    override lazy val history: History = testHistory
    override val blockStorage: BlockStorage = testBlockStorage
  }

  private val app = stub[App]

  import app.basicMessagesSpecsRepo._

  private def reasonableTimeInterval = (TestSettings.historySynchronizerTimeout.toMillis / 2) millis

  private def validateStatus(status: Status): Unit = {
    actorRef ! GetSyncStatus
    expectMsg(status)
  }

  private def assertLatestBlockFromNonSyncPeer(): Unit = {
    val peer = stub[ConnectedPeer]

    val block = blockMock(lastHistoryBlockId + 3729047)
    actorRef ! DataFromPeer(BlockMessageSpec.messageCode, block, peer)
    testCoordinator.expectMsg(AddBlock(block, Some(peer)))
  }

  private def assertThatPeerGotBlacklisted(): Unit = {
    setBlacklistExpectations(true)
    testCoordinator.expectMsg(reasonableTimeInterval, SyncFinished.unsuccessfully)
    expectMsg(BlacklistAssertion)
  }

  private def assertPeerNeverGotBlacklisted(): Unit = setBlacklistExpectations(false)

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

    testHistory.lastBlockIds _ expects TestSettings.MaxRollback returns blockIds(lastHistoryBlockId, 9) // ids come in reverse order
    actorRef ! GetExtension(Map(peer -> 0))
    expectedGetSignaturesSpec(lastHistoryBlockId, 9)

    validateStatus(GettingExtension)

    "one block is a sign to start getting blocks" in {
      sendSignatures(lastHistoryBlockId, 11)
      expectNetworkMessage(GetBlockSpec, 11)
      validateStatus(GettingBlocks)
    }

    "at least one block id in response must be among requested ones" in {
      val notInTheHistoryBlockId = lastHistoryBlockId + 1
      val notRequestedBlockFromHistoryBeginning = 1

      assertPeerNeverGotBlacklisted()

      sendSignatures(notRequestedBlockFromHistoryBeginning, notInTheHistoryBlockId)

      testCoordinator.expectMsg(reasonableTimeInterval, SyncFinished.unsuccessfully)
    }

    "become idle on timeout in GettingExtension" in {
      assertPeerNeverGotBlacklisted()

      testCoordinator.expectNoMsg(aBitLessThanTimeout)
      testCoordinator.expectMsg(SyncFinished.unsuccessfully)

      validateStatus(Idle)
    }

    "go to GettingExtension" - {

      assertLatestBlockFromNonSyncPeer()

      sendSignatures(9, lastHistoryBlockId, 11, 12, 13)

      expectedGetSignaturesSpec(13, 12)

      "sending same signatures twice should not lead to blacklisting" in {
        assertPeerNeverGotBlacklisted()
        sendSignatures(9, lastHistoryBlockId, 11, 12, 13)
      }

      "go to GettingExtensionTail" - {

        validateStatus(GettingExtensionTail)

        val validBlockIds = blockIds(13, 14, 15)

        "extension tail from another peer(s) should not lead to the peers blacklisting" in {
          assertPeerNeverGotBlacklisted()
          dataFromNetwork(SignaturesSpec, validBlockIds, stub[ConnectedPeer])
        }

        "blacklist on timeout in states following GettingExtension" in {
          testCoordinator.expectNoMsg(aBitLessThanTimeout)

          assertThatPeerGotBlacklisted()

          validateStatus(Idle)
        }

        "follow ledger download scenario" - {

          sendSignatures(validBlockIds: _*)

          expectedGetSignaturesSpec(15, 14)

          sendSignatures(14, 15)

          val finalBlockIdInterval = 11 to 15

          validateStatus(GettingBlocks)

          "react on GetExtension in the Idle state only" in {
            actorRef ! GetExtension(Map(peer -> 10000))

            validateStatus(GettingBlocks)
          }

          "blocks loading" - {

            assertLatestBlockFromNonSyncPeer()

            val numberOfBlocks = finalBlockIdInterval.size

            def setHistoryScoreExpectations(delta: BigInt): Unit =
              testHistory.score _ expects() returns (initialScore + (numberOfBlocks * blockScore) + delta) repeat (0 to numberOfBlocks)

            def sendBlocks(): Unit = {
              finalBlockIdInterval foreach { expectNetworkMessage(GetBlockSpec, _) }
              Random.shuffle(finalBlockIdInterval) foreach { id => sendBlock(blockMock(id)) }
            }

            def assertThatBlocksLoaded(): Unit = {
              assertPeerNeverGotBlacklisted()

              testCoordinator.expectMsgPF(hint = s"$numberOfBlocks fork blocks") {
                case SyncFinished(true, Some((lastCommonBlockId, blockIterator, Some(connectedPeer)))) =>
                  connectedPeer shouldBe peer
                  BlockIdExtraction.extract(lastCommonBlockId) shouldBe lastHistoryBlockId

                  val forkStorageBlockIds = blockIterator.map(id => InnerId(id.uniqueId)).toSeq
                  forkStorageBlockIds shouldBe blockIds(finalBlockIdInterval: _*).map(InnerId)
              }

              validateStatus(Idle)
            }

            "entire fork loading" - {

              setloadEntireForkChunk(true)

              "fork has two blocks better score" in {
                setHistoryScoreExpectations(-(blockScore * 2 + 1))

                sendBlocks()

                assertThatBlocksLoaded()
              }
            }

            "partial fork loading" - {

              setloadEntireForkChunk(false)

              "fork has lower score" in {
                setHistoryScoreExpectations(1)

                assertPeerNeverGotBlacklisted()

                sendBlocks()

                testCoordinator.expectMsg(reasonableTimeInterval, SyncFinished.unsuccessfully)

                validateStatus(Idle)
              }

              "fork has better score" - {

                setHistoryScoreExpectations(-1)

                "same block twice should not reset timeout" in {
                  val firstSubsequentBlockId = finalBlockIdInterval.head

                  sendBlock(blockMock(firstSubsequentBlockId))

                  Thread sleep aBitLessThanTimeout.toMillis

                  sendBlock(blockMock(firstSubsequentBlockId))

                  assertThatPeerGotBlacklisted()
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
