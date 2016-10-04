package scorex.lagonaki.integration

import org.scalatest._
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.lagonaki.mocks.BlockMock
import scorex.lagonaki.{TestingCommons, TransactionTestingCommons}
import scorex.transaction.state.database.state.AccState
import scorex.transaction.{AssetAcc, BalanceSheet, FeesStateChange, PaymentTransaction}
import scorex.utils.{ScorexLogging, _}

//TODO: Should be independed
class StoredStateSpecification extends FunSuite with TestLock with Matchers with ScorexLogging
with TransactionTestingCommons with PrivateMethodTester with OptionValues {

  import TestingCommons._

  val peers = applications.tail
  val app = peers.head
  val state = app.transactionModule.blockStorage.state
  val history = app.transactionModule.blockStorage.history
  val acc = accounts.head
  val recipient = application.wallet.privateKeyAccounts().last
  require(acc.address != recipient.address)

  test("invalidate transaction with forged signature in sequence") {
    val amount = state.asInstanceOf[BalanceSheet].balance(acc) / 1000
    val ts = System.currentTimeMillis()
    val transactions: Seq[PaymentTransaction] = (1 until 100).map { i =>
      PaymentTransaction(acc, recipient, amount, i, ts + i)
    }
    val txToForge = transactions.head
    val forgedSignature = forgeSignature(txToForge.signature)
    val forgedTransaction = PaymentTransaction(new PublicKeyAccount(txToForge.sender.publicKey), txToForge.recipient,
      txToForge.amount, txToForge.fee, txToForge.timestamp, forgedSignature)

    val transactionsToValidate = transactions :+ forgedTransaction
    val validTransactions = state.validate(transactionsToValidate)

    validTransactions.count(tx => (tx.id sameElements txToForge.signature) ||
      (tx.id sameElements forgedTransaction.signature)) shouldBe 1
    validTransactions.size should be(transactionsToValidate.size - 1)
  }


  test("balance confirmations") {
    val rec = new PrivateKeyAccount(randomBytes())
    val senderBalance = state.asInstanceOf[BalanceSheet].balance(acc)
    state.balance(rec) shouldBe 0L
    senderBalance should be > 100L

    val txs = Seq(transactionModule.createPayment(acc, rec, 5, 1))
    val block = new BlockMock(txs)
    state.processBlock(block)
    state.balance(rec) shouldBe 5L
    state.balanceWithConfirmations(rec, 1) shouldBe 0L

    state.processBlock(new BlockMock(Seq()))
    state.balance(rec) shouldBe 5L
    state.balanceWithConfirmations(rec, 1) shouldBe 5L
    state.balanceWithConfirmations(rec, 2) shouldBe 0L

    val spendingBlock = new BlockMock(Seq(transactionModule.createPayment(rec, acc, 2, 1)))
    state.processBlock(spendingBlock)
    state.balance(rec) shouldBe 2L
    state.balanceWithConfirmations(rec, 1) shouldBe 2L

    state.processBlock(new BlockMock(Seq(transactionModule.createPayment(acc, rec, 5, 1))))
    state.balance(rec) shouldBe 7L
    state.balanceWithConfirmations(rec, 3) shouldBe 2L


    state.processBlock(new BlockMock(Seq(transactionModule.createPayment(acc, rec, 5, 1))))
    state.balance(rec) shouldBe 12L
    state.balanceWithConfirmations(rec, 1) shouldBe 7L
    state.balanceWithConfirmations(rec, 2) shouldBe 2L
    state.balanceWithConfirmations(rec, 4) shouldBe 2L
    state.balanceWithConfirmations(rec, 5) shouldBe 0L
  }

  test("private methods") {
    val testAdd = "aPFwzRp5TXCzi6DSuHmpmbQunopXRuxLk"
    val testAcc = new Account(testAdd)
    val applyMethod = PrivateMethod[Unit]('applyChanges)
    state.balance(testAcc) shouldBe 0
    val tx = transactionModule.createPayment(acc, testAcc, 1, 1)
    state invokePrivate applyMethod(Map(AssetAcc(testAcc, None) ->(AccState(2L), Seq(FeesStateChange(1L), tx))))
    state.balance(testAcc) shouldBe 2
    state.included(tx).value shouldBe state.stateHeight
    state invokePrivate applyMethod(Map(AssetAcc(testAcc, None) ->(AccState(0L), Seq(tx))))
  }

  test("validate single transaction") {
    val senderBalance = state.asInstanceOf[BalanceSheet].balance(acc)
    senderBalance should be > 0L
    val nonValid = transactionModule.createPayment(acc, recipient, senderBalance, 1)
    state.isValid(nonValid) shouldBe false

    val valid = transactionModule.createPayment(acc, recipient, senderBalance - 1, 1)
    state.isValid(valid) shouldBe true
  }

  test("double spending") {
    val senderBalance = state.asInstanceOf[BalanceSheet].balance(acc)
    val doubleSpending = (1 to 2).map(i => transactionModule.createPayment(acc, recipient, senderBalance / 2, 1))
    doubleSpending.foreach(t => state.isValid(t) shouldBe true)
    state.isValid(doubleSpending) shouldBe false
    state.validate(doubleSpending).size shouldBe 1
  }

  test("validate plenty of transactions") {
    val trans = (1 to transactionModule.utxStorage.sizeLimit).map { i =>
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

  test("last transaction of account one block behind") {
    val amount = state.asInstanceOf[BalanceSheet].balance(acc) / 1000
    val tx1 = transactionModule.createPayment(acc, recipient, amount, 1)
    state.isValid(tx1) shouldBe true
    val tx2 = transactionModule.createPayment(acc, recipient, amount, 2)
    state.isValid(tx2) shouldBe true

    val block = new BlockMock(Seq(tx1, tx2))
    state.processBlock(block)

    val result = state.lastAccountLagonakiTransaction(acc)
    result.isDefined shouldBe true
    result.get shouldBe tx2
  }

  test("last transaction of account few blocks behind") {
    val amount = state.asInstanceOf[BalanceSheet].balance(acc) / 1000
    val tx1 = transactionModule.createPayment(acc, recipient, amount, 1)
    val tx2 = transactionModule.createPayment(acc, recipient, amount, 2)
    val block1 = new BlockMock(Seq(tx2, tx1))
    state.processBlock(block1)

    val tx3 = transactionModule.createPayment(recipient, acc, amount / 2, 3)
    val tx4 = transactionModule.createPayment(recipient, acc, amount / 2, 4)
    val block2 = new BlockMock(Seq(tx3, tx4))
    state.processBlock(block2)

    val result1 = state.lastAccountLagonakiTransaction(acc)
    result1.isDefined shouldBe true
    result1.get shouldBe tx2

    val result2 = state.lastAccountLagonakiTransaction(recipient)
    result2.isDefined shouldBe true
    result2.get shouldBe tx4
  }

}
