package scorex.lagonaki.props

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.lagonaki.BlockTestingCommons
import scorex.utils._

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
      val prevTx = storage.state.accountTransactions(gen).length
      storage.state.included(block.transactions.head) shouldBe None
      storage.appendBlock(block).isSuccess shouldBe true
      storage.history.height() shouldBe prevH + 1
      storage.state.accountTransactions(gen).length shouldBe prevTx + 1
      storage.state.included(block.transactions.head).get shouldBe block.uniqueId
    }
  }

  property("Don't add incorrect blocks") {
    val wrongBlockId = Some("wrong".getBytes)
    forAll { (gb: Long, gs: Array[Byte], seed: Array[Byte]) =>
      val prevTx = storage.state.accountTransactions(gen).length
      val block = genBlock(gb, gs, seed, wrongBlockId)
      val prevH = storage.history.height()
      storage.state.included(block.transactions.head) shouldBe None
      storage.appendBlock(block).isSuccess shouldBe false
      storage.state.included(block.transactions.head) shouldBe None
      storage.history.height() shouldBe prevH
      storage.state.accountTransactions(gen).length shouldBe prevTx
    }
  }

  property("Update to branch with better score") {
    val branchPoint = storage.history.lastBlock
    val senderSeed = randomBytes(32)
    val sender = new PrivateKeyAccount(senderSeed)
    val bt = 20
    val biggerBt = 19

    //Add block to best chain
    val firstBlock = genBlock(bt, randomBytes(32), senderSeed, Some(branchPoint.uniqueId))
    val firstBlockTransaction = firstBlock.transactions.head
    storage.appendBlock(firstBlock).isSuccess shouldBe true
    storage.history.lastBlock.uniqueId should contain theSameElementsAs firstBlock.uniqueId
    storage.state.accountTransactions(sender).length shouldBe 1
    storage.state.accountTransactions(sender).head.fee shouldBe bt
    storage.state.included(firstBlockTransaction).get shouldBe firstBlock.uniqueId

    //Add block with the same score to branch point
    val branchedBlock = genBlock(bt, randomBytes(32), senderSeed, Some(branchPoint.uniqueId))
    storage.appendBlock(branchedBlock).isSuccess shouldBe true
    storage.history.lastBlock.uniqueId should contain theSameElementsAs firstBlock.uniqueId
    storage.state.accountTransactions(sender).length shouldBe 1
    storage.state.accountTransactions(sender).head.fee shouldBe bt
    storage.state.included(firstBlockTransaction).get shouldBe firstBlock.uniqueId

    //Add block with the better score to branch point
    val bestBlock = genBlock(biggerBt, randomBytes(32), senderSeed, Some(branchPoint.uniqueId))
    storage.appendBlock(bestBlock).isSuccess shouldBe true
    storage.history.lastBlock.uniqueId should contain theSameElementsAs bestBlock.uniqueId
    storage.state.accountTransactions(sender).length shouldBe 1
    storage.state.accountTransactions(sender).head.fee shouldBe biggerBt
    storage.state.included(firstBlockTransaction) shouldBe None
  }
}