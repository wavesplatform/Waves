package scorex.network

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.block.Block._
import scorex.transaction.History

import scala.language.{implicitConversions, postfixOps}

class BlockchainSynchronizerObjectSpecification extends FreeSpec
  with Matchers
  with MockFactory
  with OneInstancePerTest {

  import BlockchainSynchronizer._

  "a (sub)sequience of block ids to download" - {
    "a sequience" in {
      test(Seq(1, 2, 3, 4), historyContaining(1, 2), 2, Seq(3, 4))
    }

    "all blocks are in history" in {
      test(Seq(1, 2, 3, 4), historyContaining(1, 2, 3, 4), 4, Seq())
    }

    "suspicious block id" in {
      test(Seq(1, 2, 3, 4), historyContaining(1, 3), 1, Seq(2, 3, 4))
    }

    "first block(s) are not history" in {
      blockIdsToStartDownload(Seq(10000, 2, 3, 4), historyContaining(1, 2, 3)) shouldEqual None
    }
  }

  private def test(blockIds: Seq[Int], h: History, expectedLastCommon: InnerId, expected: Seq[Int]): Unit = {
    val Some((commonId, tail)) = blockIdsToStartDownload(blockIds, h)
    commonId shouldBe expectedLastCommon
    tail should contain theSameElementsInOrderAs toInnerIds(expected)
  }

  private implicit def toInnerIds(i: Seq[Int]): InnerIds = i.map(toInnerId)
  private implicit def toInnerId(i: Int): InnerId = InnerId(Array(i.toByte))

  private def historyContaining(blockIds: Int*): History = {
    val history = mock[History]

    (history.contains(_: BlockId)) expects * onCall { id: BlockId => blockIds.contains(id.head.toInt) } anyNumberOfTimes()

    history
  }
}
