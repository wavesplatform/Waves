package scorex.consensus.mining

import akka.actor.Props
import akka.testkit.TestProbe
import com.wavesplatform.settings.WavesSettings
import scorex.ActorTestingCommons
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.app.Application
import scorex.block.Block
import scorex.consensus.ConsensusModule
import scorex.network.Coordinator.AddBlock
import scorex.transaction.{History, TransactionModule}
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}

class MinerMock(app: Application) extends Miner(app) {
  override protected def preciseTime: Long = System.currentTimeMillis()
}

class TFLikeMinerSpecification extends ActorTestingCommons {

  import System.currentTimeMillis

  import Miner._

  private val testWallet = new Wallet(None, "", Option("seed".getBytes()))
  private val account = testWallet.generateNewAccount().get

  val testCoordinator = TestProbe("Coordinator")

  private val calculatedGenDelay = 2000 millis

  private val testHistory = mock[History]
  private val testConsensusModule = mock[ConsensusModule]

  private val f = mockFunction[Block, String]
  f.expects(*).never
  (testConsensusModule.blockOrdering(_: TransactionModule)).expects(*).returns(Ordering.by(f)).anyNumberOfTimes

  private def mayBe(b: Boolean): Range = (if (b) 0 else 1) to 1

  private def setBlockGenExpectations(expected: Seq[Block], maybe: Boolean = false): Unit =
    (testConsensusModule.generateNextBlocks(_: Seq[PrivateKeyAccount])(_: TransactionModule))
      .expects(Seq(account), *)
      .returns(expected)
      .repeat(mayBe(maybe))

  private def setBlockGenTimeExpectations(block: Block, time: Option[Long], maybe: Boolean = false): Unit =
    (testConsensusModule.nextBlockGenerationTime(_: Block, _: PublicKeyAccount)(_: TransactionModule))
      .expects(block, account, *)
      .returns(time)
      .repeat(mayBe(maybe))

  private def setLastBlockExpectations(block: Block, maybe: Boolean = false): Unit = {
    (testHistory.lastBlock _).expects().returns(block).repeat(mayBe(maybe))
  }

  private def setExpectations(lastBlockId: Int, d: Option[Duration], maybe: Boolean = false): Unit = {
    val lastBlock = testBlock(lastBlockId)

    inSequence {
      setLastBlockExpectations(lastBlock, maybe)
      setBlockGenTimeExpectations(lastBlock, d.map(currentTimeMillis + _.toMillis), maybe)
    }
  }

  val wavesSettings = WavesSettings.fromConfig(baseTestConfig)

  private trait App extends ApplicationMock {
    override val settings = wavesSettings
    override val wallet: Wallet = testWallet
    override val coordinator = testCoordinator.ref
    override val history: History = testHistory
    override implicit val consensusModule: ConsensusModule = testConsensusModule
  }

  private val genTimeShift = Miner.BlockGenerationTimeShift

  protected override val actorRef = system.actorOf(Props(classOf[MinerMock], mock[App]))

  testSafely {

    val newBlock = testBlock(111)

    "TF-like scheduling approach" - {

      "block generating" - {

        setExpectations(1, Some(calculatedGenDelay))

        "stop" in {
          setBlockGenExpectations(Seq(newBlock), maybe = true)

          actorRef ! GuessABlock(false)
          Thread sleep genTimeShift.toMillis

          actorRef ! Stop

          testCoordinator.expectNoMsg(calculatedGenDelay + genTimeShift)
        }

        s"schedule gen in $calculatedGenDelay" - {

          "block is generated" in {

            setBlockGenExpectations(Seq(newBlock))

            actorRef ! GuessABlock(false)

            testCoordinator.expectNoMsg(calculatedGenDelay)
            testCoordinator.expectMsg(AddBlock(newBlock, None))
            testCoordinator.expectNoMsg(calculatedGenDelay + genTimeShift)
          }

          "block is NOT generated" in {
            setBlockGenExpectations(Seq.empty)

            setExpectations(1, Some(calculatedGenDelay))

            setBlockGenExpectations(Seq.empty, maybe = true)
            setExpectations(1, Some(calculatedGenDelay), maybe = true)

            actorRef ! GuessABlock(false)
            testCoordinator.expectNoMsg(calculatedGenDelay + genTimeShift * 2)
          }
        }
      }

      "reschedule" in {

        val veryLongDelay = calculatedGenDelay * 100

        setExpectations(1, Some(veryLongDelay))

        actorRef ! GuessABlock(false)

        val shortDelay = calculatedGenDelay / 2

        setExpectations(1, Some(shortDelay))

        setBlockGenExpectations(Seq(newBlock))

        actorRef ! GuessABlock(rescheduleImmediately = true)

        testCoordinator.expectMsg(shortDelay + calculatedGenDelay, AddBlock(newBlock, None))
      }

      "broken schedule should fallback to default" - {

        def incorrectScheduleScenario(timePoint: Long): Unit = {
          setExpectations(1, Some(timePoint).map(_ millis))
          setBlockGenExpectations(Seq(newBlock))

          actorRef ! GuessABlock(false)

          testCoordinator.expectNoMsg(wavesSettings.minerSettings.generationDelay)
          testCoordinator.expectMsg(AddBlock(newBlock, None))
          testCoordinator.expectNoMsg(wavesSettings.minerSettings.generationDelay + genTimeShift)
        }

        "past" in {
          incorrectScheduleScenario(currentTimeMillis - 10000)
        }

        "far future" in {
          incorrectScheduleScenario(MaxBlockGenerationDelay.toMillis + 174305)
        }
      }
    }
  }
}

class SimpleMinerSpecification extends ActorTestingCommons {

  import System.currentTimeMillis

  import Miner._

  private val testWallet = new Wallet(None, "", Option("seed".getBytes()))
  private val account = testWallet.generateNewAccount().get

  val testCoordinator = TestProbe("Coordinator")

  private val testHistory = mock[History]
  private val testConsensusModule = mock[ConsensusModule]

  private val f = mockFunction[Block, String]
  f.expects(*).never
  (testConsensusModule.blockOrdering(_: TransactionModule)).expects(*).returns(Ordering.by(f)).anyNumberOfTimes

  private def mayBe(b: Boolean): Range = (if (b) 0 else 1) to 1

  private def setBlockGenExpectations(expected: Seq[Block], maybe: Boolean = false): Unit =
    (testConsensusModule.generateNextBlocks(_: Seq[PrivateKeyAccount])(_: TransactionModule))
      .expects(Seq(account), *)
      .returns(expected)
      .repeat(mayBe(maybe))

  private def setBlockGenTimeExpectations(block: Block, time: Option[Long], maybe: Boolean = false): Unit =
    (testConsensusModule.nextBlockGenerationTime(_: Block, _: PublicKeyAccount)(_: TransactionModule))
      .expects(block, account, *)
      .returns(time)
      .repeat(mayBe(maybe))

  private def setLastBlockExpectations(block: Block, maybe: Boolean = false): Unit = {
    (testHistory.lastBlock _).expects().returns(block).repeat(mayBe(maybe))
  }

  private def setExpectations(lastBlockId: Int, d: Option[Duration], maybe: Boolean = false): Unit = {
    val lastBlock = testBlock(lastBlockId)

    inSequence {
      setLastBlockExpectations(lastBlock, maybe)
      setBlockGenTimeExpectations(lastBlock, d.map(currentTimeMillis + _.toMillis), maybe)
    }
  }

  val wavesSettings = WavesSettings.fromConfig(testConfigTFLikeOff)

  private trait App extends ApplicationMock {
    override val settings = wavesSettings
    override val wallet: Wallet = testWallet
    override val coordinator = testCoordinator.ref
    override val history: History = testHistory
    override implicit val consensusModule: ConsensusModule = testConsensusModule
  }

  private val genTimeShift = Miner.BlockGenerationTimeShift

  protected override val actorRef = system.actorOf(Props(classOf[MinerMock], mock[App]))

  testSafely {

    val newBlock = testBlock(111)

    "Simple (fixed time interval) scheduling" - {

      "block generating" in {

        inSequence {
          setBlockGenExpectations(Seq.empty)
          setBlockGenExpectations(Seq(newBlock))
        }

        actorRef ! GuessABlock(false)

        testCoordinator.expectNoMsg(wavesSettings.minerSettings.generationDelay * 2)
        testCoordinator.expectMsg(AddBlock(newBlock, None))
        testCoordinator.expectNoMsg()
      }
    }
  }
}