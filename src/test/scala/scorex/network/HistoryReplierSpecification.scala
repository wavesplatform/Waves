package scorex.network

import akka.actor.Props
import scorex.ActorTestingCommons
import scorex.block.Block._
import scorex.network.BlockchainSynchronizer.{InnerId, InnerIds}
import scorex.settings.SettingsMock
import scorex.transaction.History

import scala.language.implicitConversions

class HistoryReplierSpecification extends ActorTestingCommons {

  private implicit def toInnerIds(ids: Seq[Int]): InnerIds = ids.map { i => InnerId(toBlockId(i)) }

  private def mockHistory(blockIds: InnerIds): History = {
    val history = mock[History]
    history.lookForward _ expects (*,*) onCall {
      (parentSignature, howMany) =>
        blockIds.dropWhile(_ != InnerId(parentSignature)).slice(1, howMany + 1).map(_.blockId)
    } anyNumberOfTimes()
    history
  }

  private object TestSettings extends SettingsMock {
    override lazy val maxChain = 5
  }

  private val lastHistoryBlockId = 20
  private val h = mockHistory(1 to lastHistoryBlockId)

  private trait App extends ApplicationMock {
    override lazy val settings = TestSettings
    override lazy val history: History = h
  }

  private val app = stub[App]

  import app.basicMessagesSpecsRepo._

  // according to the protocol ids come in reverse order!
  private def sendSignatures(lastBlockId: Int, blockId: Int): Unit =
    dataFromNetwork(GetSignaturesSpec, (blockId to lastBlockId).reverse: BlockIds)

  private def expectedSignaturesSpec(blockIds: Seq[Int]): Unit = expectNetworkMessage(SignaturesSpec, blockIds)

  override protected val actorRef = system.actorOf(Props(classOf[HistoryReplier], app))

  testSafely {
    "return block signatures" in {
      val last = 10
      sendSignatures(last, 8) // according to the protocol ids come in reverse order!
      expectedSignaturesSpec(last to last + TestSettings.maxChain)
    }

    "history contains less block signatures than requested" in {
      sendSignatures(18, 16)
      expectedSignaturesSpec(18 to lastHistoryBlockId)
    }

    "last two block ids" in {
      sendSignatures(lastHistoryBlockId, lastHistoryBlockId - 1)
      expectedSignaturesSpec(lastHistoryBlockId - 1 to lastHistoryBlockId)
    }

    "no reply in case of last or non-exising block id" in {
      sendSignatures(lastHistoryBlockId, lastHistoryBlockId)
      sendSignatures(lastHistoryBlockId + 10, lastHistoryBlockId + 5)

      networkController.expectNoMsg(testDuration)
    }
  }
}
