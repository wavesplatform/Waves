package scorex.lagonaki.unit

import org.scalatest.{FunSuite, Matchers}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, NxtLikeConsensusModule}
import scorex.consensus.qora.{QoraLikeConsensusBlockData, QoraLikeConsensusModule}
import scorex.lagonaki.TestingCommons
import scorex.lagonaki.TestingCommons._
import scorex.transaction._
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random

class BlockSpecification extends FunSuite with Matchers with TestingCommons {

  test("Block generation then validation") {
    // Prepare tests
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
    def genValidBlock(): Block = {
      Await.result(consensusModule.generateNextBlocks(accounts)(transactionModule), 10.seconds).headOption match {
        case Some(block: Block) => block
        case None => genValidBlock()
      }
    }
    val genesisAccs = application.blockStorage.history.genesis.transactions.flatMap(_ match {
      case gtx: GenesisTransaction => Some(gtx.recipient)
      case _ => None
    })
    def genTransaction(senderAcc: PrivateKeyAccount, recipientAcc: Account, amt: Long, fee: Long = 1): Transaction = {
      transactionModule.createPayment(senderAcc, recipientAcc, amt, fee)
    }
    def genValidTransaction(randomAmnt: Boolean = true): Transaction = {
      val senderAcc = accounts(Random.nextInt(accounts.size))
      val senderBalance = transactionModule.blockStorage.state.asInstanceOf[BalanceSheet].generationBalance(senderAcc)
      val fee = Random.nextInt(5).toLong + 1
      if (senderBalance <= fee) {
        genValidTransaction(randomAmnt)
      } else {
        val amt = if (randomAmnt) Math.abs(Random.nextLong() % (senderBalance - fee))
        else senderBalance - fee
        genTransaction(senderAcc, accounts(Random.nextInt(accounts.size)), amt, fee)
      }
    }

    // Start tests
    // Gen block with transactions
    val TSize = SimpleTransactionModule.MaxTransactionsPerBlock + 1
    val transactions = (1 to TSize) map (i => genValidTransaction())
    val block2 = genValidBlock()
    block2.isValid shouldBe true
    transactionModule.transactions(block2).size shouldBe SimpleTransactionModule.MaxTransactionsPerBlock
    transactions.foreach(tx => transactionModule.blockStorage.state.included(tx) shouldBe None)
    transactionModule.blockStorage.appendBlock(block2)
    transactionModule.transactions(block2).foreach(tx => transactionModule.blockStorage.state.included(tx).get shouldBe block2.uniqueId)
    UnconfirmedTransactionsDatabaseImpl.all().foreach(tx => UnconfirmedTransactionsDatabaseImpl.remove(tx))
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe 0

    // Don't include same transactions twice
    transactions.foreach(tx => transactionModule.onNewOffchainTransaction(tx))
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe TSize
    val b3tx = genValidTransaction(false)
    val block3 = genValidBlock()
    block3.isValid shouldBe true
    transactionModule.transactions(block3).head.signature shouldBe b3tx.signature

    // Branched block with the same transaction with full balance
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

  import TestingCommons._

  test("Nxt block with txs bytes/parse roundtrip") {
    implicit val consensusModule = new NxtLikeConsensusModule()
    implicit val transactionModule = new SimpleTransactionModule()(application.settings, application)

    val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
    val gen = new PrivateKeyAccount(reference)

    val bt = Random.nextLong()
    val gs = Array.fill(NxtLikeConsensusModule.GeneratorSignatureLength)(Random.nextInt(100).toByte)

    val sender = new PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction(sender, gen, 5, 1000, System.currentTimeMillis() - 5000)

    val tbd = Seq(tx)
    val cbd = new NxtLikeConsensusBlockData {
      override val generationSignature: Array[Byte] = gs
      override val baseTarget: Long = bt
    }

    val version = 1: Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    val parsedBlock = Block.parse(block.bytes).get

    assert(parsedBlock.consensusDataField.value.asInstanceOf[NxtLikeConsensusBlockData].generationSignature.sameElements(gs))
    assert(parsedBlock.versionField.value == version)
    assert(parsedBlock.signerDataField.value.generator.publicKey.sameElements(gen.publicKey))
  }

  test("Qora block with txs bytes/parse roundtrip") {
    implicit val consensusModule = new QoraLikeConsensusModule()
    implicit val transactionModule = new SimpleTransactionModule()(application.settings, application)

    val reference = Array.fill(Block.BlockIdLength)(Random.nextInt(100).toByte)
    val gen = new PrivateKeyAccount(reference)

    val gb = Random.nextLong()
    val gs = Array.fill(QoraLikeConsensusModule.GeneratorSignatureLength)(Random.nextInt(100).toByte)

    val sender = new PrivateKeyAccount(reference.dropRight(2))
    val tx: Transaction = PaymentTransaction(sender, gen, 5, 1000, System.currentTimeMillis() - 5000)

    val tbd = Seq(tx)
    val cbd = new QoraLikeConsensusBlockData {
      override val generatorSignature: Array[Byte] = gs
      override val generatingBalance: Long = gb
    }

    val cbdBytes = consensusModule.formBlockData(cbd).bytes
    assert(cbdBytes.takeRight(QoraLikeConsensusModule.GeneratorSignatureLength).sameElements(gs))

    val version = 1: Byte
    val timestamp = System.currentTimeMillis()

    val block = Block.buildAndSign(version, timestamp, reference, cbd, tbd, gen)
    val parsedBlock = Block.parse(block.bytes).get

    val parsedCdf = parsedBlock.consensusDataField.value.asInstanceOf[QoraLikeConsensusBlockData]
    assert(parsedCdf.generatingBalance == gb)
    assert(parsedCdf.generatorSignature.sameElements(gs))
    assert(parsedBlock.versionField.value == version)
    assert(parsedBlock.signerDataField.value.generator.publicKey.sameElements(gen.publicKey))
  }

}