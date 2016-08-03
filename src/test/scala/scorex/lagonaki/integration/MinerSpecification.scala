package scorex.lagonaki.integration

import akka.actor.Props
import akka.testkit.TestProbe
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.consensus.mining.Miner
import scorex.lagonaki.ActorTestingCommons
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
    override lazy val blockGenerationDelay: FiniteDuration = 1 second
  }

  val testWallet = new Wallet(None, null, Option("seed".getBytes()))
  val account = testWallet.generateNewAccount().get

  val testCoordinator = TestProbe("Coordinator")

  val delay = 1000

  val tStart = currentTimeMillis

  val testHistory = mock[History]
  val testConsensusModule = mock[ConsensusModule[Unit]]

  val f = mockFunction[Block, String]
  f.expects(*).never
  (testConsensusModule.blockOrdering(_: TransactionModule[Unit])).expects(*).returns(Ordering.by(f))

  def setBlockForgingExpectations(expected: Seq[Block]): Unit =
    (testConsensusModule.generateNextBlocks(_: Seq[PrivateKeyAccount])(_: TransactionModule[Unit]))
      .expects(Seq(account), *)
      .returns(expected)
      .once

  def setForgingTimeExpectations(block: Block, time: Long): Unit =
    (testConsensusModule.nextBlockForgingTime(_: Block, _: PublicKeyAccount)(_: TransactionModule[Unit]))
      .expects(block, account, *)
      .returns(Some(time))
      .once

  def setLastBlockExpectations(block: Block): Unit = (testHistory.lastBlock _).expects().returns(block).once

  def setExpectations(lastBlockId: Int, t: Long, forgedBlock: Option[Block]): Unit = {
    val lastBlock = mockBlock(lastBlockId)

    inSequence {
      setLastBlockExpectations(lastBlock)
      setForgingTimeExpectations(lastBlock, t + delay)
      setBlockForgingExpectations(forgedBlock.toSeq)
    }
  }

  setExpectations(1, tStart, None)

  trait App extends ApplicationMock {
    override val settings = TestSettings
    override val wallet: Wallet = testWallet
    override val coordinator = testCoordinator.ref
    override val history: History = testHistory
    override implicit val consensusModule: ConsensusModule[Unit] = testConsensusModule
  }

  protected override val actorRef = system.actorOf(Props(classOf[Miner], mock[App]))

  testSafely {

    val maxDelay = (math.max(delay, TestSettings.blockGenerationDelay.toMillis) millis) + Miner.ForgingTimeShift

    "forge a block, then stop" in {

      testCoordinator.expectNoMsg(maxDelay * 2)

      val newBlock = mockBlock(3)
      setExpectations(2, currentTimeMillis, Some(newBlock))
      actorRef ! GuessABlock

      testCoordinator.expectNoMsg(maxDelay)
      testCoordinator.expectMsg(maxDelay, AddBlock(newBlock, None))

      actorRef ! Stop
      testCoordinator.expectNoMsg(maxDelay)
    }
  }
}
