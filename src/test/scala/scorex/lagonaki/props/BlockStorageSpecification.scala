package scorex.lagonaki.props

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.block.Block
import scorex.lagonaki.BlockTestingCommons
import scorex.transaction.BlockChain

class BlockStorageSpecification extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
with BlockTestingCommons {

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
    storage.history match {
      case b: BlockChain =>
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
      case _ =>
    }

  }


}