package scorex.lagonaki.integration

import org.scalatest._
import scorex.account.{PrivateKeyAccount, PublicKeyAccount, Account}
import scorex.lagonaki.mocks.BlockMock
import scorex.lagonaki.{TestingCommons, TransactionTestingCommons}
import scorex.transaction.state.database.state.AccState
import scorex.transaction.{BalanceSheet, FeesStateChange}
import scorex.utils.ScorexLogging
import scorex.utils._

import scala.util.Random

class StoredStateSpecification extends FunSuite with Matchers with BeforeAndAfterAll with ScorexLogging
with TransactionTestingCommons with PrivateMethodTester with OptionValues {

  import TestingCommons._

  val peers = applications.tail
  val app = peers.head
  val state = app.transactionModule.blockStorage.state
  val history = app.transactionModule.blockStorage.history
  val acc = accounts.head
  val recepient = wallet.privateKeyAccounts().last
  require(acc.address != recepient.address)

  test("balance confirmations") {
    val rec = new PrivateKeyAccount(randomBytes())
    val senderBalance = state.asInstanceOf[BalanceSheet].balance(acc.address)
    state.balance(rec.address) shouldBe 0L
    senderBalance should be > 100L

    val txs = Seq(transactionModule.createPayment(acc, rec, 5, 1))
    val block = new BlockMock(txs)
    state.processBlock(block)
    state.balance(rec.address) shouldBe 5L
    state.balanceWithConfirmations(rec.address, 1) shouldBe 0L

    state.processBlock(new BlockMock(Seq()))
    state.balance(rec.address) shouldBe 5L
    state.balanceWithConfirmations(rec.address, 1) shouldBe 5L
    state.balanceWithConfirmations(rec.address, 2) shouldBe 0L

    val spendingBlock = new BlockMock(Seq(transactionModule.createPayment(rec, acc, 2, 1)))
    state.processBlock(spendingBlock)
    state.balance(rec.address) shouldBe 2L
    state.balanceWithConfirmations(rec.address, 1) shouldBe 2L

    state.processBlock(new BlockMock(Seq(transactionModule.createPayment(acc, rec, 5, 1))))
    state.balance(rec.address) shouldBe 7L
    state.balanceWithConfirmations(rec.address, 3) shouldBe 2L


    state.processBlock(new BlockMock(Seq(transactionModule.createPayment(acc, rec, 5, 1))))
    state.balance(rec.address) shouldBe 12L
    state.balanceWithConfirmations(rec.address, 1) shouldBe 7L
    state.balanceWithConfirmations(rec.address, 2) shouldBe 2L
    state.balanceWithConfirmations(rec.address, 4) shouldBe 2L
    state.balanceWithConfirmations(rec.address, 5) shouldBe 0L


  }

  test("private methods") {
    val testAdd = "aPFwzRp5TXCzi6DSuHmpmbQunopXRuxLk"
    val applyMethod = PrivateMethod[Unit]('applyChanges)
    state.balance(testAdd) shouldBe 0
    val tx = transactionModule.createPayment(acc, new Account(testAdd), 1, 1)
    state invokePrivate applyMethod(Map(testAdd ->(AccState(2L), Seq(FeesStateChange(1L), tx))))
    state.balance(testAdd) shouldBe 2
    state.included(tx).value shouldBe state.stateHeight
    state invokePrivate applyMethod(Map(testAdd ->(AccState(0L), Seq(tx))))

  }


  test("validate single transaction") {
    val senderBalance = state.asInstanceOf[BalanceSheet].balance(acc.address)
    senderBalance should be > 0L
    val nonValid = transactionModule.createPayment(acc, recepient, senderBalance, 1)
    state.isValid(nonValid) shouldBe false

    val valid = transactionModule.createPayment(acc, recepient, senderBalance - 1, 1)
    state.isValid(valid) shouldBe true
  }

  test("double spending") {
    val senderBalance = state.asInstanceOf[BalanceSheet].balance(acc.address)
    val doubleSpending = (1 to 2).map(i => transactionModule.createPayment(acc, recepient, senderBalance / 2, 1))
    doubleSpending.foreach(t => state.isValid(t) shouldBe true)
    state.isValid(doubleSpending) shouldBe false
    state.validate(doubleSpending).size shouldBe 1
  }

  test("validate plenty of transactions") {
    val trans = (1 to transactionModule.utxStorage.SizeLimit).map { i =>
      genValidTransaction()
    }
    profile(state.validate(trans)) should be < 1000L
    state.validate(trans).size should be <= trans.size
  }

  test("included") {
    val incl = includedTransactions(history.lastBlock, history)
    incl.nonEmpty shouldBe true
    incl.forall(t => state.included(t).isDefined) shouldBe true

    val newTx = genValidTransaction()
    state.included(newTx).isDefined shouldBe false

  }

}
