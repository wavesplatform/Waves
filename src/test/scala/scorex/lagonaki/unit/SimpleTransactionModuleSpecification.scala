package scorex.lagonaki.unit

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.WavesSettings
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import scorex.app.Application
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.lagonaki.mocks.{ConsensusMock, TestBlock}
import scorex.settings.TestBlockchainSettings
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{PaymentTransaction, SimpleTransactionModule, Transaction}
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.postfixOps
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

  implicit val app = stub[MyApp]
  implicit val settings = wavesSettings
  implicit val consensusModule = app.consensusModule
  implicit val transactionModule = new SimpleTransactionModule(TestBlockchainSettings.Enabled.genesisSettings)
  val genesisTimestamp = System.currentTimeMillis()
  if (transactionModule.blockStorage.history.isEmpty) {
    transactionModule.blockStorage.appendBlock(Block.genesis(consensusModule.genesisData, transactionModule.genesisData, genesisTimestamp))
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
    assert(transactionModule.validate(validTx).isRight)

    val oldTx = PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp - (1 day).toMillis).right.get
    assert(transactionModule.validate(oldTx).isLeft)
  }

  val validDelegate = (tx: Transaction) => Right(tx)

  test("clearIncorrectTransactions() removes valid but expired txs") {
    transactionModule.utxStorage.all().foreach(transactionModule.utxStorage.remove)

    // prepare
    val validTx = PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp).right.get
    val oldValidTx = PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp - (1 day).toMillis).right.get
    transactionModule.utxStorage.putIfNew(validTx, validDelegate)
    transactionModule.utxStorage.putIfNew(oldValidTx, validDelegate)
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
    transactionModule.utxStorage.putIfNew(validTx, validDelegate)
    transactionModule.utxStorage.putIfNew(invalidTx, validDelegate)
    assert(transactionModule.utxStorage.all().size == 2)

    // do
    transactionModule.clearIncorrectTransactions()

    // assert
    assert(transactionModule.utxStorage.all().size == 1)
    assert(!transactionModule.utxStorage.all().contains(invalidTx))
  }

  test("unique txs by id in one block") {
    val tx = TransferTransaction.create(None, privateKeyAccount, privateKeyAccount, 1L, genesisTimestamp + 1000, None, 100000L, Array.empty).right.get
    transactionModule.isValid(TestBlock(Seq(tx))) shouldBe true
    val replaySeq = Seq(tx, tx)
    transactionModule.isValid(TestBlock(replaySeq)) shouldBe false
  }

  test("packUnconfirmed() packs txs in correct order") {
    transactionModule.utxStorage.all().foreach(transactionModule.utxStorage.remove)

    val correctSeq = Seq(
      PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp + 2).right.get,
      PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp + 1).right.get,
      PaymentTransaction.create(privateKeyAccount, privateKeyAccount, 1L, 100000L, genesisTimestamp).right.get
    )

    Random.shuffle(correctSeq).foreach(t => transactionModule.utxStorage.putIfNew(t, (t: Transaction) => Right(t)))
    assert(transactionModule.utxStorage.all().size == 3)
    assert(transactionModule.packUnconfirmed() == correctSeq)
  }
}
