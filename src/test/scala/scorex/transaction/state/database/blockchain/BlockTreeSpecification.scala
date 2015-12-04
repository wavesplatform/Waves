package scorex.transaction.state.database.blockchain

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.lagonaki.TestingCommons
import scorex.transaction.{PaymentTransaction, SimpleTransactionModule, Transaction}

import scala.util.{Failure, Success, Random}

class BlockTreeSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
with TestingCommons {

  implicit val consensusModule = new NxtLikeConsensusModule()
  implicit val transactionModule = new SimpleTransactionModule()
  val blockTree = new StoredBlockTree(None)

  val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
  val gen = new PrivateKeyAccount(reference)
  val genesis = Block.genesis()

  val blockGen: Gen[Block] = for {
    gb <- Arbitrary.arbitrary[Long]
    gs <- Arbitrary.arbitrary[Array[Byte]]
    seed <- Arbitrary.arbitrary[Array[Byte]]
  } yield genBlock(gb, gs, seed)


  property("Add genesis") {
    blockTree.height() shouldBe 0
    blockTree.appendBlock(genesis)
    blockTree.height() shouldBe 1
  }

  property("Add blocks in chain") {
    blockTree.height() shouldBe 1

    forAll(blockGen) { (block: Block) =>
      val prevH = blockTree.height()
      val prevS = blockTree.score()
      val prevB = blockTree.lastBlock

      blockTree.appendBlock(block).isSuccess shouldBe true

      blockTree.height() shouldBe prevH + 1
      blockTree.score() shouldBe prevS + consensusModule.blockScore(block)
      blockTree.lastBlock.uniqueId  should contain theSameElementsAs block.uniqueId
      blockTree.parent(block).get.uniqueId  should contain theSameElementsAs prevB.uniqueId
      blockTree.contains(block) shouldBe true
      blockTree.contains(prevB.uniqueId) shouldBe true
    }


  }

  var lastBlockId: BlockId = genesis.uniqueId

  def genBlock(bt: Long, gs: Array[Byte], seed: Array[Byte], parentId: Option[BlockId] = None)
              (implicit consensusModule: NxtLikeConsensusModule, transactionModule: SimpleTransactionModule): Block = {

    val reference = parentId.getOrElse(lastBlockId)

    val sender = new PrivateKeyAccount(seed)
    val tx: Transaction = PaymentTransaction(sender, gen, 5, 1000, System.currentTimeMillis() - 5000)

    val tbd = Seq(tx)
    val cbd = new NxtLikeConsensusBlockData {
      override val generationSignature: Array[Byte] = gs
      override val baseTarget: Long = math.max(math.abs(bt), 1)
    }

    val version = 1: Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    lastBlockId = block.uniqueId
    block
  }

}