package scorex.lagonaki.integration

import org.scalatest._
import scorex.account.Account
import scorex.lagonaki.{TestingCommons, TransactionTestingCommons}
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.state.database.state.AccState
import scorex.transaction.{BalanceSheet, FeesStateChange}
import scorex.utils.ScorexLogging

import scala.util.Random

class StoredStateSpecification extends FunSuite with Matchers with BeforeAndAfterAll with ScorexLogging
  with TransactionTestingCommons with PrivateMethodTester with OptionValues {

  import TestingCommons._

  val peers = applications.tail
  val app = peers.head
  val state = app.transactionModule.blockStorage.state
  val history = app.transactionModule.blockStorage.history
  val acc = accounts.head
  val recepient = accounts.last

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
    val trans = (1 to UnconfirmedTransactionsDatabaseImpl.SizeLimit).map { i =>
      val account = accounts(Random.nextInt(accounts.size))
      val senderBalance = state.asInstanceOf[BalanceSheet].balance(account.address)
      senderBalance should be > 0L
      val amount = Random.nextLong() % senderBalance
      val fee = Random.nextLong() % senderBalance
      transactionModule.createPayment(acc, recepient, amount, 1)
    }
    val st = System.currentTimeMillis()
    state.validate(trans).size should be <= trans.size
    //TODO optimization
    //    System.currentTimeMillis() - st should be <= 2000L
  }

  test("included") {
    val incl = includedTransactions(history.lastBlock, history)
    incl.nonEmpty shouldBe true
    incl.forall(t => state.included(t).isDefined) shouldBe true

    val newTx = genValidTransaction()
    state.included(newTx).isDefined shouldBe false

  }

}
