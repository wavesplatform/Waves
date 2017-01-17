package scorex.lagonaki.unit

import java.net.InetSocketAddress

import org.scalamock.scalatest.MockFactory
import org.scalatest.FunSuite
import play.api.libs.json.{JsObject, Json}
import scorex.app.Application
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.lagonaki.mocks.ConsensusMock
import scorex.settings.{Settings, WavesHardForkParameters}
import scorex.transaction.{PaymentTransaction, SimpleTransactionModule, TransactionSettings}
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.postfixOps

//TODO: gagarin55 - Can't move it to appropriate module due to dependancy on some ConsesusModule impl
class SimpleTransactionModuleSpecification extends FunSuite with MockFactory {

  object MySettings extends TransactionSettings with Settings {
    override lazy val settingsJSON: JsObject = Json.obj()
    override lazy val dataDirOpt: Option[String] = None
    override lazy val knownPeers = Seq.empty[InetSocketAddress]
  }

  trait MyApp extends Application {
    override val settings: Settings = MySettings
    override implicit val consensusModule = new ConsensusMock
  }

  val forkParameters = new AnyRef with WavesHardForkParameters {
    override def allowTemporaryNegativeUntil: Long = 0L
    override def requireSortedTransactionsAfter: Long = Long.MaxValue
    override def allowInvalidPaymentTransactionsByTimestamp: Long = Long.MaxValue
    override def generatingBalanceDepthFrom50To1000AfterHeight: Long = Long.MaxValue
    override def minimalGeneratingBalanceAfterTimestamp: Long = Long.MaxValue
    override def allowTransactionsFromFutureUntil: Long = Long.MaxValue
    override def allowUnissuedAssetsUntil: Long = Long.MaxValue
    override def allowBurnTransactionAfterTimestamp: Long = Long.MaxValue
    override def requirePaymentNotIncludedAfterTimestamp: Long = Long.MaxValue
  }

  implicit val app = stub[MyApp]
  implicit val settings = MySettings
  implicit val consensusModule = app.consensusModule
  implicit val transactionModule = new SimpleTransactionModule(forkParameters)
  val genesisTimestamp = System.currentTimeMillis()
  if (transactionModule.blockStorage.history.isEmpty) {
    transactionModule.blockStorage.appendBlock(Block.genesis(genesisTimestamp))
  }
  assert(!transactionModule.blockStorage.history.isEmpty)

  // account with money
  val walletSeed = Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz").get
  val privateKeyAccount = Wallet.generateNewAccount(walletSeed, -1)
  assert(transactionModule.blockStorage.state.balance(privateKeyAccount) > 0L)
  // account without money
  val noBalanceAccount = Wallet.generateNewAccount(walletSeed, 5)
  assert(transactionModule.blockStorage.state.balance(noBalanceAccount) == 0L)


  test("isValid() checks that tx not too old") {
    val validTx = PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp).right.get
    assert(transactionModule.isValid(validTx, validTx.timestamp))

    val oldTx = PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp - (1 day).toMillis).right.get
    assert(!transactionModule.isValid(oldTx, oldTx.timestamp))
  }

  test("clearIncorrectTransactions() removes valid but expired txs") {
    transactionModule.utxStorage.all().foreach(transactionModule.utxStorage.remove)

    // prepare
    val validTx = PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp).right.get
    val oldValidTx = PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp - (1 day).toMillis).right.get
    transactionModule.utxStorage.putIfNew(validTx)
    transactionModule.utxStorage.putIfNew(oldValidTx)
    assert(transactionModule.utxStorage.all().size == 2)

    // do
    transactionModule.clearIncorrectTransactions()

    // assert
    assert(transactionModule.utxStorage.all().size == 1)
    assert(!transactionModule.utxStorage.all().contains(oldValidTx))
  }

  test("clearIncorrectTransactions() removes not expired but invalid txs") {
    transactionModule.utxStorage.all().foreach(transactionModule.utxStorage.remove)
    // prepare
    val validTx = PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp).right.get
    val invalidTx = PaymentTransaction.create(noBalanceAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp).right.get
    transactionModule.utxStorage.putIfNew(validTx)
    transactionModule.utxStorage.putIfNew(invalidTx)
    assert(transactionModule.utxStorage.all().size == 2)

    // do
    transactionModule.clearIncorrectTransactions()

    // assert
    assert(transactionModule.utxStorage.all().size == 1)
    assert(!transactionModule.utxStorage.all().contains(invalidTx))
  }
}
