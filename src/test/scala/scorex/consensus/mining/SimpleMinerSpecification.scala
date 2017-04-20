package scorex.consensus.mining

import akka.actor.Props
import akka.testkit.TestProbe
import com.wavesplatform.settings.WavesSettings
import scorex.ActorTestingCommons
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.WavesConsensusModule
import scorex.network.Coordinator.AddBlock
import scorex.transaction.{History, TransactionModule}
import scorex.wallet.Wallet

import scala.concurrent.duration.Duration


class SimpleMinerSpecification extends ActorTestingCommons {

  import System.currentTimeMillis

  import Miner._

  private val testWallet = new Wallet(None, "", Option("seed".getBytes()))
  private val account = testWallet.generateNewAccount().get

  val testCoordinator = TestProbe("Coordinator")

  val wavesSettings: WavesSettings = WavesSettings.fromConfig(testConfigTFLikeOff)
  private val testHistory = mock[History]

  class MockableConsensusModule extends WavesConsensusModule(wavesSettings.blockchainSettings)

  private val testConsensusModule = mock[MockableConsensusModule]

  private val f = mockFunction[Block, String]
  f.expects(*).never()
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
    (testHistory.blockAt _).expects(*).returns(Some(block)).repeat(mayBe(maybe))
  }

  private def setExpectations(lastBlockId: Int, d: Option[Duration], maybe: Boolean = false): Unit = {
    val lastBlock = testBlock(lastBlockId)

    inSequence {
      setLastBlockExpectations(lastBlock, maybe)
      setBlockGenTimeExpectations(lastBlock, d.map(currentTimeMillis + _.toMillis), maybe)
    }
  }

  private trait App extends ApplicationMock {
    override val settings = wavesSettings
    override val wallet: Wallet = testWallet
    override val coordinator = testCoordinator.ref
    override val historyOverride: History = testHistory
    override implicit val consensusModule: WavesConsensusModule = testConsensusModule
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
