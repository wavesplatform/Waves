package scorex.network

import java.util.concurrent.locks.ReentrantReadWriteLock

import akka.actor.{ActorRef, Props}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.HistoryTest
import org.h2.mvstore.MVStore
import scorex.ActorTestingCommons
import scorex.block.Block._
import scorex.lagonaki.mocks.TestBlock
import scorex.network.BlockchainSynchronizer.{InnerId, InnerIds}
import scorex.network.message._
import scorex.transaction.History

import scala.language.implicitConversions

class HistoryReplierSpecification extends ActorTestingCommons with HistoryTest {

  private implicit def toInnerIds(ids: Seq[Int]): InnerIds = ids.map { i => InnerId(toBlockId(i)) }

  private def mockHistory(blocks: Int): History = {
    val history = new HistoryWriterImpl(new MVStore.Builder().open(), new ReentrantReadWriteLock())
    appendGenesisBlock(history)
    for (i <- 1 until blocks) appendTestBlock(history)
    history
  }

  private val localConfig = ConfigFactory.parseString(
    """
      |waves {
      |  synchronization {
      |    max-chain-length: 5
      |  }
      |}
    """.stripMargin).withFallback(baseTestConfig).resolve()

  val wavesSettings = WavesSettings.fromConfig(localConfig)

  private val lastHistoryBlockId = 200
  private val h = mockHistory(lastHistoryBlockId)

  // according to the protocol ids come in reverse order!
  private def sendSignatures(f: Int, t: Int): Unit =
    sendSignatures((t to f by -1).flatMap(h.blockAt).map(_.uniqueId))

  private def sendSignatures(ids: BlockIds): Unit =
    dataFromNetwork(GetSignaturesSpec, ids) // according to the protocol ids come in reverse order!

  private def expectedSignaturesSpec(blockIds: Seq[Int]): Unit = expectNetworkMessage(SignaturesSpec, blocksHeightRangeToTestExtractedValues(blockIds))

  override protected val actorRef = system.actorOf(Props(new HistoryReplier(networkControllerMock, h, wavesSettings.synchronizationSettings.maxChainLength: Int)))

  private def blocksHeightRangeToTestExtractedValues(s: Seq[Int]) =
    s.map(i => h.blockAt(i).get.uniqueId(0).toInt)

  testSafely {
    "return block signatures" in {
      sendSignatures(8, 10)
      expectedSignaturesSpec(10 to 10 + wavesSettings.synchronizationSettings.maxChainLength)
    }

    "history contains less block signatures than requested" in {
      sendSignatures(196, 198)
      expectedSignaturesSpec(198 to lastHistoryBlockId)
    }

    "last two block ids" in {
      sendSignatures(lastHistoryBlockId - 1, lastHistoryBlockId)
      expectedSignaturesSpec(lastHistoryBlockId - 1 to lastHistoryBlockId)
    }

    "no reply in case of last or non-exising block id" in {
      sendSignatures(Seq(h.blockAt(lastHistoryBlockId).get.uniqueId))
      sendSignatures(Seq(TestBlock.randomSignature, TestBlock.randomSignature, TestBlock.randomSignature))

      networkController.expectNoMsg(testDuration)
    }
  }
}
