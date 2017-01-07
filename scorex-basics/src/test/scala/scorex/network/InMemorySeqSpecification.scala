package scorex.network

import org.h2.mvstore.MVStore
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.block.Block
import scorex.block.Block._
import scorex.consensus.ConsensusModule
import scorex.network.BlockchainSynchronizer.{InnerId, _}
import scorex.transaction.TransactionModule

import scala.language.{implicitConversions, postfixOps}

class InMemorySeqSpecification extends FreeSpec
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

  class InMemoryBlockSeqMock extends InMemoryBlockSeq {
    override protected[this] def toBytes(block: Block): Array[Byte] = block.uniqueId
    override protected[this] def fromBytes(bytes: Array[Byte]): Option[Block] = Some(mockBlock(bytes))
  }

  val imMemoryBlockSeq = new InMemoryBlockSeqMock

  "life cycle" in {
    imMemoryBlockSeq.initialize(Seq(1, 2, 3, 4, 5), 100)
    imMemoryBlockSeq.cumulativeBlockScore shouldBe 100

    imMemoryBlockSeq.containsBlockId(1) shouldBe true
    imMemoryBlockSeq.containsBlockId(111) shouldBe false

    imMemoryBlockSeq.numberOfBlocks shouldBe 0

    val veryFirstBlock = mockBlock(1)

    imMemoryBlockSeq.addIfNotContained(veryFirstBlock) shouldBe true
    imMemoryBlockSeq.addIfNotContained(mockBlock(1)) shouldBe false

    imMemoryBlockSeq.cumulativeBlockScore shouldBe 101

    imMemoryBlockSeq.addIfNotContained(mockBlock(2)) shouldBe true
    imMemoryBlockSeq.cumulativeBlockScore shouldBe 102

    imMemoryBlockSeq.addIfNotContained(mockBlock(4)) shouldBe true
    imMemoryBlockSeq.cumulativeBlockScore shouldBe 102

    imMemoryBlockSeq.addIfNotContained(mockBlock(3)) shouldBe true
    imMemoryBlockSeq.cumulativeBlockScore shouldBe 104

    val lastBlock = mockBlock(5)
    imMemoryBlockSeq.addIfNotContained(lastBlock)

    imMemoryBlockSeq.numberOfBlocks shouldBe 5

    imMemoryBlockSeq.noIdsWithoutBlock shouldBe true
    val blocks = imMemoryBlockSeq.blocksInOrder.toSeq
    blocks.size shouldBe 5

    blocks.head shouldBe veryFirstBlock
    blocks.last shouldBe lastBlock

    imMemoryBlockSeq.initialize(Seq(13), 1000)
    imMemoryBlockSeq.cumulativeBlockScore shouldBe 1000
    imMemoryBlockSeq.allIdsWithoutBlock shouldBe Seq(InnerId(13))
  }
}
