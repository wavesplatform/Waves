package scorex.network

import org.scalatest.{FreeSpec, Matchers}
import scorex.account.PublicKeyAccount
import scorex.block.Block._
import scorex.block.{Block, SignerData}
import scorex.consensus.nxt.{NxtConsensusBlockField, NxtLikeConsensusBlockData}
import scorex.network.BlockchainSynchronizer.InnerId
import scorex.transaction.TransactionsBlockField
import scorex.transaction.TypedTransaction.SignatureLength

import scala.language.{implicitConversions, postfixOps}

class InMemorySeqSpecification extends FreeSpec  with Matchers {

  def toBlockId(i: Int): BlockId = Array(i.toByte)

  private def newBlock(referenceId: Int) =
     Block(0, 1, toBlockId(referenceId), SignerData(new PublicKeyAccount(Array.fill(32)(0)), Array()),
      NxtLikeConsensusBlockData(1L, Array.fill(SignatureLength)(0: Byte)), Seq.empty)

  "life cycle" in {
    val imMemoryBlockSeq = new InMemoryBlockSeq(Seq(1, 2, 3, 4, 5).map(i => InnerId(toBlockId(i))))

    imMemoryBlockSeq.cumulativeBlockScore() shouldBe 0

    imMemoryBlockSeq.containsBlockId(toBlockId(1)) shouldBe true
    imMemoryBlockSeq.containsBlockId(toBlockId(111)) shouldBe false

    imMemoryBlockSeq.numberOfBlocks shouldBe 0

    val veryFirstBlock = newBlock(1)

    imMemoryBlockSeq.addIfNotContained(veryFirstBlock) shouldBe true
    imMemoryBlockSeq.addIfNotContained(newBlock(1)) shouldBe false

    val lastBlock = newBlock(5)
    imMemoryBlockSeq.addIfNotContained(lastBlock)

    imMemoryBlockSeq.numberOfBlocks shouldBe 1

    imMemoryBlockSeq.noIdsWithoutBlock shouldBe false
  }
}
