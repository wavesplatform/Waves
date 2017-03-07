package scorex.lagonaki.unit

import scala.concurrent.duration._
import scala.language.postfixOps
import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.WavesSettings
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import scorex.account.AddressScheme
import scorex.app.{Application, RunnableApplication}
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.lagonaki.mocks.{ConsensusMock, TestBlock}
import scorex.settings.{ChainParameters, TestChainParameters}
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{PaymentTransaction, SimpleTransactionModule, Transaction}
import scorex.wallet.Wallet

import scala.util.Random

//TODO: gagarin55 - Can't move it to appropriate module due to dependancy on some ConsesusModule impl
class SimpleTransactionModuleSpecification extends FunSuite with MockFactory with Matchers {

  private val config = ConfigFactory.parseString(
    """
      |waves {
      |  directory: ""
      |  network {
      |    file: ""
      |    known-peers = []
      |  }
      |  blockchain {
      |    file: ""
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()

  val wavesSettings = WavesSettings.fromConfig(config)

  trait MyApp extends Application {
    override val settings: WavesSettings = wavesSettings
    override implicit val consensusModule = new ConsensusMock
  }

  val forkParameters = new ChainParameters with TestChainParameters.GenesisData {
    override def allowTemporaryNegativeUntil: Long = 0L

    override def requireSortedTransactionsAfter: Long = Long.MaxValue

    override def allowInvalidPaymentTransactionsByTimestamp: Long = Long.MaxValue

    override def generatingBalanceDepthFrom50To1000AfterHeight: Long = Long.MaxValue

    override def minimalGeneratingBalanceAfterTimestamp: Long = Long.MaxValue

    override def allowTransactionsFromFutureUntil: Long = Long.MaxValue

    override def allowUnissuedAssetsUntil: Long = Long.MaxValue

    override def allowBurnTransactionAfterTimestamp: Long = Long.MaxValue

    override def allowLeaseTransactionAfterTimestamp: Long = Long.MaxValue

    override def requirePaymentUniqueId: Long = Long.MaxValue

    override def initialBalance: Long = 100000000000000L

    override def genesisTimestamp: Long = ???

    override def addressScheme: AddressScheme = ???

    override def allowExchangeTransactionAfterTimestamp: Long = 0L
  }

  implicit val app = stub[MyApp]
  implicit val settings = wavesSettings
  implicit val consensusModule = app.consensusModule
  implicit val transactionModule = new SimpleTransactionModule(forkParameters)
  val genesisTimestamp = System.currentTimeMillis()
  if (transactionModule.blockStorage.history.isEmpty) {
    transactionModule.blockStorage.appendBlock(Block.genesis(RunnableApplication.consensusGenesisBlockData, transactionModule.genesisData, genesisTimestamp))
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

  test("unique txs by id in one block") {
    val tx = TransferTransaction.create(None, privateKeyAccount, privateKeyAccount, 1L, genesisTimestamp + 1000, None, 100000L, Array.empty).right.get
    transactionModule.isValid(tx, tx.timestamp) shouldBe true
    val replaySeq = Seq(tx, tx)
    transactionModule.isValid(TestBlock(replaySeq)) shouldBe false
  }

  test("packUnconfirmed() packs txs in correct order") {
    val correctSeq = Seq(
      PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp + 2).right.get,
      PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp + 1).right.get,
      PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp).right.get
    )

    Random.shuffle(correctSeq).foreach(t => transactionModule.utxStorage.putIfNew(t))
    assert(transactionModule.utxStorage.all().size == 3)
    assert(transactionModule.packUnconfirmed() == correctSeq)
  }
}
