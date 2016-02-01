package scorex.lagonaki.integration

import org.scalatest.{FunSuite, Matchers}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.block.Block
import scorex.lagonaki.TestingCommons
import scorex.lagonaki.TestingCommons._
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction._
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

class BlockGenerationSpecification extends FunSuite with Matchers with TestingCommons {


  implicit val consensusModule = application.consensusModule
  implicit val transactionModule = application.transactionModule
  if (transactionModule.blockStorage.history.isEmpty) {
    transactionModule.blockStorage.appendBlock(Block.genesis())
  }
  val wallet = application.wallet
  if (wallet.privateKeyAccounts().isEmpty) {
    wallet.generateNewAccounts(3)
  }
  val accounts = wallet.privateKeyAccounts()
  val ab = accounts.map(a => transactionModule.blockStorage.state.asInstanceOf[BalanceSheet].generationBalance(a)).sum
  require(ab > 2)

  def genValidBlock(): Block = {
    Await.result(consensusModule.generateNextBlocks(accounts)(transactionModule), 10.seconds).headOption match {
      case Some(block: Block) if block.isValid => block
      case None =>
        Thread.sleep(500)
        genValidBlock()
    }
  }

  val genesisAccs = application.blockStorage.history.genesis.transactions.flatMap(_ match {
    case gtx: GenesisTransaction => Some(gtx.recipient)
    case _ => None
  })

  def genValidTransaction(randomAmnt: Boolean = true): Transaction = {
    val senderAcc = accounts(Random.nextInt(accounts.size))
    val senderBalance = transactionModule.blockStorage.state.asInstanceOf[BalanceSheet].generationBalance(senderAcc)
    val fee = Random.nextInt(5).toLong + 1
    if (senderBalance <= fee) {
      genValidTransaction(randomAmnt)
    } else {
      val amt = if (randomAmnt) Math.abs(Random.nextLong() % (senderBalance - fee))
      else senderBalance - fee
      val tx = transactionModule.createPayment(senderAcc, accounts(Random.nextInt(accounts.size)), amt, fee)
      if (tx.validate()(transactionModule) == ValidationResult.ValidateOke) tx
      else genValidTransaction(randomAmnt)
    }
  }

  var blokc2txs: Seq[Transaction] = Seq.empty

  test("Gen block with transactions") {
    // Start tests
    // Gen block with transactions
    val TSize = SimpleTransactionModule.MaxTransactionsPerBlock * 4
    val transactions = (1 to TSize) map (i => genValidTransaction())
    val block2 = genValidBlock()
    block2.isValid shouldBe true
    assert(transactionModule.transactions(block2).size <= SimpleTransactionModule.MaxTransactionsPerBlock)
    transactions.foreach(tx => transactionModule.blockStorage.state.included(tx) shouldBe None)
    transactionModule.blockStorage.appendBlock(block2)
    blokc2txs = transactionModule.transactions(block2)
    blokc2txs.foreach(tx => transactionModule.blockStorage.state.included(tx).get shouldBe block2.uniqueId)
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
    transactionModule.blockStorage.state.included(b3tx).get shouldBe block3.uniqueId
    block3.isValid shouldBe true
    //TODO fix and uncomment
    //    block4.isValid shouldBe true

  }

  test("Double spending") {
    UnconfirmedTransactionsDatabaseImpl.all().foreach(tx => UnconfirmedTransactionsDatabaseImpl.remove(tx))
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe 0
    accounts.foreach { a =>
      val recepient = new PublicKeyAccount(Array.empty)
      val senderBalance = transactionModule.blockStorage.state.asInstanceOf[BalanceSheet].generationBalance(a)

      if (senderBalance > 1) (1 to 2) map (i => transactionModule.createPayment(a, recepient, senderBalance / 2, 1))
    }
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe accounts.size * 2
    val block5 = genValidBlock()
    block5.isValid shouldBe true
    accounts foreach (a => assert(transactionModule.blockStorage.state.balance(a.address) > 0))
  }


}