package scorex.lagonaki.props

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.lagonaki.TestingCommons
import scorex.transaction.{PaymentTransaction, SimpleTransactionModule, Transaction}

import scala.util.{Failure, Random}

class BlockStorageSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
with TestingCommons {

  implicit val consensusModule = new NxtLikeConsensusModule()
  implicit val transactionModule = new SimpleTransactionModule()

  val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
  val gen = new PrivateKeyAccount(reference)
  val genesis = Block.genesis()

  //  val smallInteger: Gen[Int] = Gen.choose(0, 2)
  val blockGen: Gen[Block] = for {
    gb <- Arbitrary.arbitrary[Long]
    gs <- Arbitrary.arbitrary[Array[Byte]]
    seed <- Arbitrary.arbitrary[Array[Byte]]
  } yield genBlock(gb, gs, seed)

  val storage = transactionModule.blockStorage
  storage.appendBlock(genesis)

  property("Add correct blocks") {
    forAll(blockGen) { (block: Block) =>
      val prevH = storage.history.height()
      storage.appendBlock(block).isSuccess shouldBe true
      storage.history.height() shouldBe prevH + 1
    }
  }

  property("Don't add incorrect blocks") {
    val wrongBlockId = Some("wrong".getBytes)
    forAll { (gb: Long, gs: Array[Byte], seed: Array[Byte]) =>
      val block = genBlock(gb, gs, seed, wrongBlockId)
      val prevH = storage.history.height()
      storage.appendBlock(block).isSuccess shouldBe false
      storage.history.height() shouldBe prevH
    }
  }

  property("Remove after") {
    forAll { (gb: Long, gs: Array[Byte], seed: Array[Byte]) =>
      lastBlockId = storage.history.lastBlock.uniqueId
      val block = genBlock(gb, gs, seed, Some(lastBlockId))
      val prevH = storage.history.height()
      storage.appendBlock(block).isSuccess shouldBe true
      storage.history.height() shouldBe prevH + 1
      storage.removeAfter(block.referenceField.value)
      storage.history.height() shouldBe prevH
    }
    storage.history.height() should be > 1
    storage.removeAfter(genesis.uniqueId)
    storage.history.height() shouldBe 1

  }


  var lastBlockId: BlockId = genesis.uniqueId

  def genBlock(bt: Long, gs: Array[Byte], seed: Array[Byte], parentId: Option[BlockId] = None)
              (implicit consensusModule: NxtLikeConsensusModule, transactionModule: SimpleTransactionModule): Block = {

    val reference = parentId.getOrElse(lastBlockId)
    val gen = new PrivateKeyAccount(reference)

    val sender = new PrivateKeyAccount(seed)
    val tx: Transaction = PaymentTransaction(sender, gen, 5, 1000, System.currentTimeMillis() - 5000)

    val tbd = Seq(tx)
    val cbd = new NxtLikeConsensusBlockData {
      override val generationSignature: Array[Byte] = gs
      override val baseTarget: Long = bt
    }

    val version = 1: Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    lastBlockId = block.uniqueId
    block
  }

}