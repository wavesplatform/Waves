package scorex.transaction.state.database.blockchain

import org.h2.mvstore.MVStore
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.block.Block
import scorex.block.Block._
import scorex.consensus.ConsensusModule
import scorex.network.BlockchainSynchronizer.{InnerId, _}
import scorex.transaction.TransactionModule

import scala.language.{implicitConversions, postfixOps}

class StoredBlockSeqSpecification extends FreeSpec
  with Matchers
  with OneInstancePerTest
  with MockFactory {

  protected def blockIds(ids: Int*): BlockIds = ids.map(toBlockId)
  protected implicit def toBlockIds(ids: Seq[Int]): BlockIds = blockIds(ids:_*)
  protected implicit def toBlockId(i: Int): BlockId = Array(i.toByte)

  private implicit def toInnerIds(i: Seq[Int]): InnerIds = i.map(toInnerId)
  private implicit def toInnerId(i: Int): InnerId = InnerId(toBlockId(i))

  private def mockBlock[Id](id: Id)(implicit conv: Id => BlockId): Block = {
    trait BlockMock extends Block {
      override type ConsensusDataType = Unit
      override type TransactionDataType = Unit

      override val uniqueId: BlockId = id
    }
    mock[BlockMock]
  }

  private implicit val transactionModule = mock[TransactionModule[Unit]]
  private implicit val consensusModule = mock[ConsensusModule[Unit]]
  consensusModule.blockScore _ expects * returns 1 anyNumberOfTimes

  class StoredBlockSeqMock extends StoredBlockSeq(new MVStore.Builder().open()) {
    override protected[this] def toBytes(block: Block): Array[Byte] = block.uniqueId
    override protected[this] def fromBytes(bytes: Array[Byte]): Option[Block] = Some(mockBlock(bytes))
  }

  val storedBlockSeq = new StoredBlockSeqMock

  "life cycle" in {
    storedBlockSeq.initialize(Seq(1, 2, 3, 4, 5), 100)
    storedBlockSeq.cumulativeBlockScore shouldBe 100

    storedBlockSeq.containsBlockId(1) shouldBe true
    storedBlockSeq.containsBlockId(111) shouldBe false

    storedBlockSeq.numberOfBlocks shouldBe 0

    val veryFirstBlock = mockBlock(1)

    storedBlockSeq.addIfNotContained(veryFirstBlock) shouldBe true
    storedBlockSeq.addIfNotContained(mockBlock(1)) shouldBe false

    storedBlockSeq.cumulativeBlockScore shouldBe 101

    storedBlockSeq.addIfNotContained(mockBlock(2)) shouldBe true
    storedBlockSeq.cumulativeBlockScore shouldBe 102

    storedBlockSeq.addIfNotContained(mockBlock(4)) shouldBe true
    storedBlockSeq.cumulativeBlockScore shouldBe 102

    storedBlockSeq.addIfNotContained(mockBlock(3)) shouldBe true
    storedBlockSeq.cumulativeBlockScore shouldBe 104

    val lastBlock = mockBlock(5)
    storedBlockSeq.addIfNotContained(lastBlock)

    storedBlockSeq.numberOfBlocks shouldBe 5

    storedBlockSeq.noIdsWithoutBlock shouldBe true
    val blocks = storedBlockSeq.blocksInOrder.toSeq
    blocks.size shouldBe 5

    blocks.head shouldBe veryFirstBlock
    blocks.last shouldBe lastBlock

    storedBlockSeq.initialize(Seq(13), 1000)
    storedBlockSeq.cumulativeBlockScore shouldBe 1000
    storedBlockSeq.firstIdWithoutBlock shouldBe Some(InnerId(13))
  }
}
