package scorex.lagonaki.integration

import com.wavesplatform.settings.Constants
import org.scalatest._
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.lagonaki.TransactionTestingCommons
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.state.database.state.AccState
import scorex.transaction.state.database.state.extension.IncrementingTimestampValidator
import scorex.transaction.state.wallet.{IssueRequest, TransferRequest}
import scorex.transaction.{AssetAcc, FeesStateChange, PaymentTransaction}
import scorex.utils._

import scala.util.Random

class StoredStateSpecification extends FunSuite with Matchers with TransactionTestingCommons with PrivateMethodTester with OptionValues {

  private val state = application.transactionModule.blockStorage.state
  private val history = application.transactionModule.blockStorage.history
  private val acc = applicationNonEmptyAccounts.head
  private val recipient = applicationEmptyAccounts.head
  private val incrementingTimestampValidator: IncrementingTimestampValidator = state.validators.filter(_.isInstanceOf[IncrementingTimestampValidator]).head.asInstanceOf[IncrementingTimestampValidator]

  require(acc.address != recipient.address)

  override def beforeAll(): Unit = {
    super.beforeAll()
    Thread.sleep(1000)
    waitForSingleConnection(application)
    waitForNextBlock(application)
    Thread.sleep(1000)
  }

  test("invalidate transaction with forged signature in sequence") {
    val amount = state.balance(acc) / 1000
    val ts = System.currentTimeMillis()
    val transactions: Seq[PaymentTransaction] = (1 until 100).map { i =>
      PaymentTransaction.create(acc, recipient, amount, i, ts + i).right.get
    }
    val txToForge = transactions.head
    val forgedSignature = forgeSignature(txToForge.signature)
    val forgedTransaction = PaymentTransaction.create(new PublicKeyAccount(txToForge.sender.publicKey), txToForge.recipient,
      txToForge.amount, txToForge.fee, txToForge.timestamp, forgedSignature).right.get

    val transactionsToValidate = transactions :+ forgedTransaction
    val validTransactions = state.validate(transactionsToValidate, blockTime = transactionsToValidate.map(_.timestamp).max)

    validTransactions.count(tx => (tx.id sameElements txToForge.signature) ||
      (tx.id sameElements forgedTransaction.signature)) shouldBe 1
    validTransactions.size should be(transactionsToValidate.size - 1)
  }


  test("balance confirmations") {
    val rec = new PrivateKeyAccount(randomBytes())
    val senderBalance = state.balance(acc)
    state.balance(rec) shouldBe 0L
    senderBalance should be > 100L

    val txs = Seq(transactionModule.createPayment(acc, rec, 5, 1).right.get)
    val block = TestBlock(txs)
    state.processBlock(block)
    state.balance(rec) shouldBe 5L
    state.balanceWithConfirmations(rec, 1) shouldBe 0L

    state.processBlock(TestBlock(Seq()))
    state.balance(rec) shouldBe 5L
    state.balanceWithConfirmations(rec, 1) shouldBe 5L
    state.balanceWithConfirmations(rec, 2) shouldBe 0L

    val spendingBlock = TestBlock(Seq(transactionModule.createPayment(rec, acc, 2, 1).right.get))
    state.processBlock(spendingBlock)
    state.balance(rec) shouldBe 2L
    state.balanceWithConfirmations(rec, 1) shouldBe 2L

    state.processBlock(TestBlock(Seq(transactionModule.createPayment(acc, rec, 5, 1).right.get)))
    state.balance(rec) shouldBe 7L
    state.balanceWithConfirmations(rec, 3) shouldBe 2L


    state.processBlock(TestBlock(Seq(transactionModule.createPayment(acc, rec, 5, 1).right.get)))
    state.balance(rec) shouldBe 12L
    state.balanceWithConfirmations(rec, 1) shouldBe 7L
    state.balanceWithConfirmations(rec, 2) shouldBe 2L
    state.balanceWithConfirmations(rec, 4) shouldBe 2L
    state.balanceWithConfirmations(rec, 5) shouldBe 0L
  }

  test("private methods") {
    val testAcc = applicationEmptyAccounts.head
    val applyChanges = PrivateMethod[Unit]('applyChanges)
    state.balance(testAcc) shouldBe 0
    val tx = transactionModule.createPayment(acc, testAcc, 1, 1).right.get
    state invokePrivate applyChanges(Map(AssetAcc(testAcc, None) -> (AccState(2L), Seq(FeesStateChange(1L), tx))), NTP.correctedTime())
    state.balance(testAcc) shouldBe 2
    state.included(tx.id).value shouldBe state.stateHeight
    state invokePrivate applyChanges(Map(AssetAcc(testAcc, None) -> (AccState(0L), Seq(tx))), NTP.correctedTime())
  }

  test("validate single transaction") {
    val senderBalance = state.balance(acc)
    senderBalance should be > 0L

    transactionModule.createPayment(acc, recipient, senderBalance, 1) shouldBe 'left

    transactionModule.createPayment(acc, recipient, senderBalance - 1, 1) shouldBe 'right
  }

  test("double spending") {
    val senderBalance = state.balance(acc)
    val doubleSpending = (1 to 2).map(i => transactionModule.createPayment(acc, recipient, senderBalance / 2, 1).right.get)
    doubleSpending.foreach(t => state.isValid(t, t.timestamp) shouldBe true)
    state.isValid(doubleSpending, blockTime = doubleSpending.map(_.timestamp).max) shouldBe false
    state.validate(doubleSpending, blockTime = doubleSpending.map(_.timestamp).max).size shouldBe 1
    state.processBlock(TestBlock(doubleSpending)) should be('failure)
  }

  test("many transactions") {
    val senderBalance = state.balance(acc)

    val recipients = Seq(
      new PrivateKeyAccount(Array(34.toByte, 1.toByte)),
      new PrivateKeyAccount(Array(1.toByte, 23.toByte))
    )

    require(senderBalance > 10 * recipients.size * Constants.UnitsInWave)

    val issueAssetTx = transactionModule.issueAsset(IssueRequest(acc.address, "AAAAB", "BBBBB", 1000000, 2, reissuable = false, 100000000), application.wallet).right.get

    waitForNextBlock(application)

    val assetId = Some(Base58.encode(issueAssetTx.assetId))

    val txs = recipients.flatMap(r => Seq.fill(10)({
      Thread.sleep(1000)
      transactionModule.transferAsset(TransferRequest(assetId, None, 10, 100000, acc.address, Some("123"), r.address), application.wallet)
    }))

    txs.size should be(20)

    val shuffledTxs = Random.shuffle(txs).map(_.right.get)

    shuffledTxs.size should be(20)

    waitForNextBlock(application)

    state.assetBalance(AssetAcc(acc, Some(issueAssetTx.assetId))) should be(999800)

    recipients.foreach(r => state.assetBalance(AssetAcc(r, Some(issueAssetTx.assetId))) should be(100))
  }

  test("included") {
    val incl = includedTransactions(history.lastBlock, history)
    incl.nonEmpty shouldBe true
    incl.forall(t => state.included(t.id).isDefined) shouldBe true

    val newTx = genValidTransaction()
    state.included(newTx.id).isDefined shouldBe false
  }

  test("last transaction of account one block behind") {
    val amount = state.balance(acc) / 1000
    val tx1 = transactionModule.createPayment(acc, recipient, amount, 1).right.get
    state.isValid(tx1, tx1.timestamp) shouldBe true
    val tx2 = transactionModule.createPayment(acc, recipient, amount, 2).right.get
    state.isValid(tx2, tx2.timestamp) shouldBe true

    val block = TestBlock(Seq(tx1, tx2))
    state.processBlock(block)

    val result = state.incrementingTimestampValidator.lastAccountPaymentTransaction(acc)
    result.isDefined shouldBe true
    result.get shouldBe tx2
  }

  test("last transaction of account few blocks behind") {
    val amount = state.balance(acc) / 1000
    val tx1 = transactionModule.createPayment(acc, recipient, amount, 1).right.get
    val tx2 = transactionModule.createPayment(acc, recipient, amount, 2).right.get
    val block1 = TestBlock(Seq(tx2, tx1))
    state.processBlock(block1)

    val tx3 = transactionModule.createPayment(recipient, acc, amount / 2, 3).right.get
    val tx4 = transactionModule.createPayment(recipient, acc, amount / 2, 4).right.get
    val block2 = TestBlock(Seq(tx3, tx4))
    state.processBlock(block2)

    val result1 = state.incrementingTimestampValidator.lastAccountPaymentTransaction(acc)
    result1.isDefined shouldBe true
    result1.get shouldBe tx2

    val result2 = state.incrementingTimestampValidator.lastAccountPaymentTransaction(recipient)
    result2.isDefined shouldBe true
    result2.get shouldBe tx4
  }

  test("valid order match transaction with fully executed orders") {
    val wavesBal = state.assetBalance(AssetAcc(acc, None))
    val bal2 = state.assetBalance(AssetAcc(new Account("3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K"), None))
    val bal3 = state.assetBalance(AssetAcc(new Account("3N6dsnfD88j5yKgpnEavaaJDzAVSRBRVbMY"), None))
    wavesBal should be > 0L
  }

  test("asset distribution initial") {
    val issueAssetTx = transactionModule.issueAsset(IssueRequest(acc.address, "AAAAB", "BBBBB", 1000000, 2, reissuable = false, 100000000), application.wallet).right.get
    val block = TestBlock(Seq(issueAssetTx))
    state.processBlock(block)
    val distribution = state.assetDistribution(issueAssetTx.assetId)
    distribution shouldBe Map(acc.address -> 1000000)
  }

  test("asset distribution 2") {
    val issueAssetTx = transactionModule.issueAsset(IssueRequest(acc.address, "1234", "12345", 1000000, 2, reissuable = false, 100000000), application.wallet).right.get
    val block = TestBlock(Seq(issueAssetTx))
    state.processBlock(block)

    val transferRequest = TransferRequest(Some(Base58.encode(issueAssetTx.id)), None, 300000, 100000000, acc.address, None, recipient.address)
    val transferAssetTx = transactionModule.transferAsset(transferRequest, application.wallet).right.get
    val block2 = TestBlock(Seq(transferAssetTx))
    state.processBlock(block2)
    val distribution = state.assetDistribution(issueAssetTx.assetId)
    distribution shouldBe Map(acc.address -> 700000, recipient.address -> 300000)
  }
}
