package scorex.lagonaki.integration

import org.scalatest.{FunSuite, Matchers}
import scorex.account.PublicKeyAccount
import scorex.lagonaki.TransactionTestingCommons
import scorex.transaction._
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl

class BlockGenerationSpecification extends FunSuite with Matchers with TransactionTestingCommons {

  var blokc2txs: Seq[Transaction] = Seq.empty
  val state = transactionModule.blockStorage.state
  val history = transactionModule.blockStorage.history

  test("Gen block with transactions") {
    // Start tests
    // Gen block with transactions
    val TSize = SimpleTransactionModule.MaxTransactionsPerBlock * 4
    val transactions = (1 to TSize) map (i => genValidTransaction())
    val block2 = genValidBlock()
    block2.isValid shouldBe true
    assert(transactionModule.transactions(block2).size <= SimpleTransactionModule.MaxTransactionsPerBlock)
    transactions.foreach(tx => state.included(tx) shouldBe None)
    transactionModule.blockStorage.appendBlock(block2)
    blokc2txs = transactionModule.transactions(block2)
    blokc2txs.foreach(tx => state.included(tx) shouldBe history.heightOf(block2))
    UnconfirmedTransactionsDatabaseImpl.all().foreach(tx => UnconfirmedTransactionsDatabaseImpl.remove(tx))
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe 0
  }

  test("Don't include same transactions twice") {
    blokc2txs.foreach(tx => transactionModule.onNewOffchainTransaction(tx))
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe blokc2txs.size
    val b3tx = genValidTransaction(randomAmnt = false)
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe blokc2txs.size + 1
    val block3 = genValidBlock()
    block3.isValid shouldBe true
    val blokc3txs = transactionModule.transactions(block3)
    blokc3txs.size shouldBe 1
    blokc3txs.head.signature shouldBe b3tx.signature

    transactionModule.onNewOffchainTransaction(b3tx)
    val block4 = genValidBlock()
    block4.isValid shouldBe true
    transactionModule.transactions(block4).head.signature shouldBe b3tx.signature

    // branched block is still valid after apply of another one
    transactionModule.blockStorage.appendBlock(block3)
    state.included(b3tx).get shouldBe history.heightOf(block3)
    block3.isValid shouldBe true
    //TODO fix and uncomment
    //    block4.isValid shouldBe true

  }

  test("Double spending") {
    UnconfirmedTransactionsDatabaseImpl.all().foreach(tx => UnconfirmedTransactionsDatabaseImpl.remove(tx))
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe 0
    accounts.foreach { a =>
      val recepient = new PublicKeyAccount(Array.empty)
      val senderBalance = state.asInstanceOf[BalanceSheet].generationBalance(a)

      if (senderBalance > 1) (1 to 2) map (i => transactionModule.createPayment(a, recepient, senderBalance / 2, 1))
    }
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe accounts.size * 2
    val block5 = genValidBlock()
    block5.isValid shouldBe true
    accounts foreach (a => assert(state.balance(a.address) > 0))
  }

  test("Rollback state") {
    genValidTransaction()
    val st1 = state.hash
    val h = history.height()
    val lastBlockId = history.lastBlock.uniqueId

    val blockToRollback = genValidBlock()
    transactionModule.blockStorage.appendBlock(blockToRollback)

    history.height() shouldBe h + 1
    blockToRollback.isValid shouldBe true
    blockToRollback.transactions.nonEmpty shouldBe true
    state.hash should not be st1

    transactionModule.blockStorage.removeAfter(lastBlockId)

    history.lastBlock.uniqueId shouldBe lastBlockId
    state.hash shouldBe st1
  }


}