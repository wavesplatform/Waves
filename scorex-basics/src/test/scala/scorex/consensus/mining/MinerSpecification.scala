package scorex.consensus.mining

import akka.actor.Props
import akka.testkit.TestProbe
import scorex.ActorTestingCommons
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.network.Coordinator.AddBlock
import scorex.settings.SettingsMock
import scorex.transaction.{History, TransactionModule}
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}

class MinerSpecification extends ActorTestingCommons {

  import System.currentTimeMillis

  import Miner._

  object TestSettings extends SettingsMock {
    override lazy val blockGenerationDelay: FiniteDuration = 1500 millis
  }

  val testWallet = new Wallet(None, null, Option("seed".getBytes()))
  val account = testWallet.generateNewAccount().get

  val testCoordinator = TestProbe("Coordinator")

  val blockGenDelay = 2000 millis

  val testHistory = mock[History]
  val testConsensusModule = mock[ConsensusModule[Unit]]

  val f = mockFunction[Block, String]
  f.expects(*).never
  (testConsensusModule.blockOrdering(_: TransactionModule[Unit])).expects(*).returns(Ordering.by(f)).anyNumberOfTimes

  def setBlockGenExpectations(expected: Seq[Block]): Unit =
    (testConsensusModule.generateNextBlocks(_: Seq[PrivateKeyAccount])(_: TransactionModule[Unit]))
      .expects(Seq(account), *)
      .returns(expected)
      .noMoreThanOnce

  def setBlockGenTimeExpectations(block: Block, time: Option[Long]): Unit =
    (testConsensusModule.nextBlockGenerationTime(_: Block, _: PublicKeyAccount)(_: TransactionModule[Unit]))
      .expects(block, account, *)
      .returns(time)
      .once

  def setLastBlockExpectations(block: Block): Unit = {
    (testHistory.lastBlock _).expects().returns(block).once
  }

  def setExpectations(lastBlockId: Int, d: Option[Duration], generatedBlock: Option[Block]): Unit = {
    val lastBlock = blockMock(lastBlockId)

    inSequence {
      setLastBlockExpectations(lastBlock)
      setBlockGenTimeExpectations(lastBlock, d.map(currentTimeMillis + _.toMillis))
      setBlockGenExpectations(generatedBlock.toSeq)
    }
  }

  trait App extends ApplicationMock {
    override val settings = TestSettings
    override val wallet: Wallet = testWallet
    override val coordinator = testCoordinator.ref
    override val history: History = testHistory
    override implicit val consensusModule: ConsensusModule[Unit] = testConsensusModule
  }

  protected override val actorRef = system.actorOf(Props(classOf[Miner], mock[App]))

  testSafely {

    "generate blocks" - {

      setExpectations(1, Some(blockGenDelay), None)

      actorRef ! GuessABlock
      testCoordinator.expectNoMsg(blockGenDelay * 2)

      "broken schedule shoud fallback to default" in {
        val lastBlock = blockMock(3)
        val newBlock = blockMock(111)

        inSequence {
          setLastBlockExpectations(lastBlock)
          setBlockGenTimeExpectations(lastBlock, Some(currentTimeMillis - 10000))
          setBlockGenExpectations(Seq(newBlock))
        }
        setExpectations(3, None, None)

        actorRef ! GuessABlock

        testCoordinator.expectNoMsg(TestSettings.blockGenerationDelay)
        testCoordinator.expectMsg(AddBlock(newBlock, None))
      }

      "scheduling" - {
        val firstNewBlock = blockMock(101)
        setExpectations(2, Some(blockGenDelay), Some(firstNewBlock))

        actorRef ! GuessABlock

        testCoordinator.expectNoMsg(blockGenDelay)
        testCoordinator.expectMsg(AddBlock(firstNewBlock, None))

        val secondNewBlock = blockMock(102)
        setExpectations(3, None, Some(secondNewBlock))
        setExpectations(3, None, None)

        actorRef ! GuessABlock

        testCoordinator.expectNoMsg(TestSettings.blockGenerationDelay)
        testCoordinator.expectMsg(AddBlock(secondNewBlock, None))

        "repeat" in {
          val thirdNewBlock = blockMock(103)
          setExpectations(3, None, Some(thirdNewBlock))

          testCoordinator.expectNoMsg(TestSettings.blockGenerationDelay)
          testCoordinator.expectMsg(AddBlock(thirdNewBlock, None))
        }

        "stop" in {
          actorRef ! Stop
          testCoordinator.expectNoMsg(TestSettings.blockGenerationDelay + Miner.BlockGenerationTimeShift)
        }
      }
    }
  }
}
