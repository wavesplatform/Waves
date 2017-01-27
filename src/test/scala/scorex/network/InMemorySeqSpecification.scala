package scorex.network

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.account.PublicKeyAccount
import scorex.block.{Block, SignerData}
import scorex.block.Block._
import scorex.consensus.ConsensusModule
import scorex.network.BlockchainSynchronizer.{InnerId, _}

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
    abstract class BlockMock extends Block(0,1,conv(id),SignerData(new PublicKeyAccount(Array.fill(32)(0)),Array())) {
      override type ConsensusDataType = Unit
      override type TransactionDataType = Unit

      override val uniqueId: BlockId = id
    }
    mock[BlockMock]
  }

  private implicit val consensusModule = mock[ConsensusModule[Unit]]
  consensusModule.blockScore _ expects * returns 1 anyNumberOfTimes


  "life cycle" in {
    val imMemoryBlockSeq = new InMemoryBlockSeq(Seq(1, 2, 3, 4, 5))

    imMemoryBlockSeq.cumulativeBlockScore(100, consensusModule) shouldBe 100

    imMemoryBlockSeq.containsBlockId(1) shouldBe true
    imMemoryBlockSeq.containsBlockId(111) shouldBe false

    imMemoryBlockSeq.numberOfBlocks shouldBe 0

    val veryFirstBlock = mockBlock(1)

    imMemoryBlockSeq.addIfNotContained(veryFirstBlock) shouldBe true
    imMemoryBlockSeq.addIfNotContained(mockBlock(1)) shouldBe false

    imMemoryBlockSeq.cumulativeBlockScore(100, consensusModule) shouldBe 101

    imMemoryBlockSeq.addIfNotContained(mockBlock(2)) shouldBe true
    imMemoryBlockSeq.cumulativeBlockScore(100, consensusModule) shouldBe 102

    imMemoryBlockSeq.addIfNotContained(mockBlock(4)) shouldBe true
    imMemoryBlockSeq.cumulativeBlockScore(100, consensusModule) shouldBe 102

    imMemoryBlockSeq.addIfNotContained(mockBlock(3)) shouldBe true
    imMemoryBlockSeq.cumulativeBlockScore(100, consensusModule) shouldBe 104

    val lastBlock = mockBlock(5)
    imMemoryBlockSeq.addIfNotContained(lastBlock)

    imMemoryBlockSeq.numberOfBlocks shouldBe 5

    imMemoryBlockSeq.noIdsWithoutBlock shouldBe true
    val blocks = imMemoryBlockSeq.blocksInOrder.toSeq
    blocks.size shouldBe 5

    blocks.head shouldBe veryFirstBlock
    blocks.last shouldBe lastBlock
  }
}
