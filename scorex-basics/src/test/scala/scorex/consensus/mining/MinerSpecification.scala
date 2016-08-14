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

  private val tf = mockFunction[Boolean]
  private def setTF(value: Boolean) = tf expects() returns value anyNumberOfTimes()

  private object TestSettings extends SettingsMock {
    override lazy val blockGenerationDelay: FiniteDuration = 1500 millis
    override lazy val tflikeScheduling: Boolean = tf()
  }

  private val testWallet = new Wallet(None, "", Option("seed".getBytes()))
  private val account = testWallet.generateNewAccount().get

  val testCoordinator = TestProbe("Coordinator")

  private val calculatedGenDelay = 2000 millis

  private val testHistory = mock[History]
  private val testConsensusModule = mock[ConsensusModule[Unit]]

  private val f = mockFunction[Block, String]
  f.expects(*).never
  (testConsensusModule.blockOrdering(_: TransactionModule[Unit])).expects(*).returns(Ordering.by(f)).anyNumberOfTimes

  private def setBlockGenExpectations(expected: Seq[Block]): Unit =
    (testConsensusModule.generateNextBlocks(_: Seq[PrivateKeyAccount])(_: TransactionModule[Unit]))
      .expects(Seq(account), *)
      .returns(expected)
      .once

  private def setBlockGenTimeExpectations(block: Block, time: Option[Long]): Unit =
    (testConsensusModule.nextBlockGenerationTime(_: Block, _: PublicKeyAccount)(_: TransactionModule[Unit]))
      .expects(block, account, *)
      .returns(time)
      .once

  private def setLastBlockExpectations(block: Block): Unit = {
    (testHistory.lastBlock _).expects().returns(block).once
  }

  private def setExpectations(lastBlockId: Int, d: Option[Duration]): Unit = {
    val lastBlock = blockMock(lastBlockId)

    inSequence {
      setLastBlockExpectations(lastBlock)
      setBlockGenTimeExpectations(lastBlock, d.map(currentTimeMillis + _.toMillis))
    }
  }

  private trait App extends ApplicationMock {
    override val settings = TestSettings
    override val wallet: Wallet = testWallet
    override val coordinator = testCoordinator.ref
    override val history: History = testHistory
    override implicit val consensusModule: ConsensusModule[Unit] = testConsensusModule
  }

  private val genTimeShift = Miner.BlockGenerationTimeShift

  protected override val actorRef = system.actorOf(Props(classOf[Miner], mock[App]))

  testSafely {

    val newBlock = blockMock(111)

    "Simple (fixed time interval) scheduling" - {

      setTF(false)

      "block generating" in {

        inSequence {
          setBlockGenExpectations(Seq.empty)
          setBlockGenExpectations(Seq(newBlock))
        }

        actorRef ! GuessABlock

        testCoordinator.expectNoMsg(TestSettings.blockGenerationDelay * 2)
        testCoordinator.expectMsg(AddBlock(newBlock, None))
        testCoordinator.expectNoMsg()
      }
    }

    "TF-like scheduling approach" - {

      setTF(true)

      "block generating" - {

        setExpectations(1, Some(calculatedGenDelay))

        "stop" in {
          actorRef ! GuessABlock
          Thread sleep genTimeShift.toMillis
          actorRef ! Stop
          testCoordinator.expectNoMsg(calculatedGenDelay + genTimeShift)
        }

        s"schedule gen in $calculatedGenDelay" - {

          "block is generated" in {

            setBlockGenExpectations(Seq(newBlock))

            actorRef ! GuessABlock

            testCoordinator.expectNoMsg(calculatedGenDelay)
            testCoordinator.expectMsg(AddBlock(newBlock, None))
            testCoordinator.expectNoMsg(calculatedGenDelay +  genTimeShift)
          }

          "block is NOT generated" in {
            setBlockGenExpectations(Seq.empty)

            setExpectations(1, Some(calculatedGenDelay))

            actorRef ! GuessABlock
            testCoordinator.expectNoMsg(calculatedGenDelay + genTimeShift * 2)
          }
        }
      }

      "broken schedule should fallback to default" in {

        setExpectations(1, Some(currentTimeMillis - 10000).map(_ millis))
        setBlockGenExpectations(Seq(newBlock))

        actorRef ! GuessABlock

        testCoordinator.expectNoMsg(TestSettings.blockGenerationDelay)
        testCoordinator.expectMsg(AddBlock(newBlock, None))
        testCoordinator.expectNoMsg(TestSettings.blockGenerationDelay + genTimeShift)
      }
    }
  }
}
