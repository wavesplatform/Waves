package scorex.transaction.state.database.blockchain

import java.io.File

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.block.Block
import scorex.lagonaki.BlockTestingCommons
import scorex.utils._

class BlockTreeSpecification extends PropSpec with PropertyChecks
  with GeneratorDrivenPropertyChecks with Matchers with BlockTestingCommons {

  val dirName = "/tmp/scorex/test/"
  val dir = new File(dirName)
  dir.mkdir()
  for (file <- dir.listFiles) file.delete

  testTree(new StoredBlockTree(Some(dirName)), "File")
  testTree(new StoredBlockTree(None), "Memory")

  def testTree(blockTree: StoredBlockTree, prefix: String): Unit = {

    val blockGen: Gen[Block] = for {
      gb <- Arbitrary.arbitrary[Long]
      gs <- Arbitrary.arbitrary[Array[Byte]]
      seed <- Arbitrary.arbitrary[Array[Byte]]
    } yield genBlock(gb, gs, seed)

    property(s"$prefix: Add genesis") {
      blockTree.height() shouldBe 0
      blockTree.appendBlock(genesis).isSuccess shouldBe true
      blockTree.height() shouldBe 1
    }

    property(s"$prefix: Add linear blocks in chain") {
      blockTree.height() shouldBe 1
      lastBlockId = blockTree.lastBlock.uniqueId

      forAll(blockGen) { (block: Block) =>
        val prevH = blockTree.height()
        val prevS = blockTree.score()
        val prevB = blockTree.lastBlock

        blockTree.appendBlock(block).isSuccess shouldBe true

        blockTree.height() shouldBe prevH + 1
        blockTree.score() shouldBe prevS + consensusModule.blockScore(block)
        blockTree.lastBlock.uniqueId should contain theSameElementsAs block.uniqueId
        blockTree.parent(block).get.uniqueId should contain theSameElementsAs prevB.uniqueId
        blockTree.contains(block) shouldBe true
        blockTree.contains(prevB.uniqueId) shouldBe true
      }
    }

    property(s"$prefix: Add non-linear blocks in tree") {
      val branchPoint = blockTree.lastBlock

      //Add block to best chain
      val block = genBlock(20, randomBytes(32), randomBytes(32), Some(branchPoint.uniqueId))
      blockTree.appendBlock(block).isSuccess shouldBe true
      blockTree.lastBlock.uniqueId should contain theSameElementsAs block.uniqueId

      //Add block with the same score to branch point
      val branchedBlock = genBlock(20, randomBytes(32), randomBytes(32), Some(branchPoint.uniqueId))
      blockTree.appendBlock(branchedBlock).isSuccess shouldBe true
      blockTree.lastBlock.uniqueId should contain theSameElementsAs block.uniqueId

      //Add block with the better score to branch point
      val bestBlock = genBlock(19, randomBytes(32), randomBytes(32), Some(branchPoint.uniqueId))
      blockTree.appendBlock(bestBlock).isSuccess shouldBe true
      blockTree.lastBlock.uniqueId should contain theSameElementsAs bestBlock.uniqueId

      //Add block to subtree with smaller score to make it best subtree
      val longerTreeBlock = genBlock(19, randomBytes(32), randomBytes(32), Some(branchedBlock.uniqueId))
      blockTree.appendBlock(longerTreeBlock).isSuccess shouldBe true
      blockTree.lastBlock.uniqueId should contain theSameElementsAs longerTreeBlock.uniqueId
    }

    property(s"$prefix: Wrong block") {
      val prevS = blockTree.score()
      val prevB = blockTree.lastBlock
      val wrongBlock = genBlock(19, randomBytes(32), randomBytes(32), Some(randomBytes(51)))

      //Block with no parent in blockTree
      blockTree.appendBlock(wrongBlock).isSuccess shouldBe false
      blockTree.score() shouldBe prevS
      blockTree.lastBlock.uniqueId should contain theSameElementsAs prevB.uniqueId

      //Apply same block twice
      blockTree.appendBlock(prevB).isSuccess shouldBe false
      blockTree.score() shouldBe prevS
      blockTree.lastBlock.uniqueId should contain theSameElementsAs prevB.uniqueId
    }

  }
}