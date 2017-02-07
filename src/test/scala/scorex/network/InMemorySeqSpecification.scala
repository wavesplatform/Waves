package scorex.network

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import scorex.account.PublicKeyAccount
import scorex.block.{Block, SignerData}
import scorex.block.Block._
import scorex.consensus.ConsensusModule
import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData}
import scorex.network.BlockchainSynchronizer.{InnerId, _}
import scorex.transaction.TransactionsBlockField
import scorex.transaction.TypedTransaction.SignatureLength

import scala.language.{implicitConversions, postfixOps}

class InMemorySeqSpecification extends FreeSpec
  with Matchers
  with OneInstancePerTest
  with MockFactory {

  protected def blockIds(ids: Int*): BlockIds = ids.map(toBlockId)

  protected implicit def toBlockIds(ids: Seq[Int]): BlockIds = blockIds(ids: _*)

  protected implicit def toBlockId(i: Int): BlockId = Array(i.toByte)

  private implicit def toInnerIds(i: Seq[Int]): InnerIds = i.map(toInnerId)

  private implicit def toInnerId(i: Int): InnerId = InnerId(toBlockId(i))

  private def newBlock[Id](id: Id)(implicit conv: Id => BlockId): Block =
     Block(0, 1, conv(id), SignerData(new PublicKeyAccount(Array.fill(32)(0)), Array()),
      NxtConsensusBlockField(NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte))), TransactionsBlockField(Seq.empty))

  private implicit val consensusModule = mock[ConsensusModule]

  "life cycle" in {
    val imMemoryBlockSeq = new InMemoryBlockSeq(Seq(1, 2, 3, 4, 5))

    imMemoryBlockSeq.cumulativeBlockScore() shouldBe 100

    imMemoryBlockSeq.containsBlockId(1) shouldBe true
    imMemoryBlockSeq.containsBlockId(111) shouldBe false

    imMemoryBlockSeq.numberOfBlocks shouldBe 0

    val veryFirstBlock = newBlock(1)

    imMemoryBlockSeq.addIfNotContained(veryFirstBlock) shouldBe true
    imMemoryBlockSeq.addIfNotContained(newBlock(1)) shouldBe false
    imMemoryBlockSeq.addIfNotContained(newBlock(2)) shouldBe true
    imMemoryBlockSeq.addIfNotContained(newBlock(4)) shouldBe true
    imMemoryBlockSeq.addIfNotContained(newBlock(3)) shouldBe true

    val lastBlock = newBlock(5)
    imMemoryBlockSeq.addIfNotContained(lastBlock)

    imMemoryBlockSeq.numberOfBlocks shouldBe 5

    imMemoryBlockSeq.noIdsWithoutBlock shouldBe true
    val blocks = imMemoryBlockSeq.blocksInOrder.toSeq
    blocks.size shouldBe 5

    blocks.head shouldBe veryFirstBlock
    blocks.last shouldBe lastBlock
  }
}
