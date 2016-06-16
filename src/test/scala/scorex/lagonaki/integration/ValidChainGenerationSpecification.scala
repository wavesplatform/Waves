package scorex.lagonaki.integration

import akka.pattern.ask
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scorex.account.PublicKeyAccount
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController._
import scorex.lagonaki.{TestingCommons, TransactionTestingCommons}
import scorex.transaction.BalanceSheet
import scorex.utils.{ScorexLogging, untilTimeout}

import scala.concurrent.Await
import scala.concurrent.duration._

class ValidChainGenerationSpecification extends FunSuite with Matchers with BeforeAndAfterAll with ScorexLogging
with TransactionTestingCommons {

  import TestingCommons._

  implicit val timeout = Timeout(1.second)

  val peers = applications.tail
  val app = peers.head
  val state = app.transactionModule.blockStorage.state
  val history = app.transactionModule.blockStorage.history

  def waitGenerationOfBlocks(howMany: Int): Unit = {
    val height = maxHeight()
    untilTimeout(5.minutes, 1.seconds) {
      val heights = peers.map(_.blockStorage.history.height())
      log.info(s"Current heights are: $heights. Waiting for ${height + howMany}")
      heights.foreach(_ should be >= height + howMany)
    }
  }

  def maxHeight(): Int = peers.map(_.blockStorage.history.height()).max

  def cleanTransactionPool(): Unit = untilTimeout(1.second) {
    transactionModule.utxStorage.all().foreach(tx => transactionModule.utxStorage.remove(tx))
    transactionModule.utxStorage.all().size shouldBe 0
  }

  test("generate 10 blocks and synchronize") {
    val genBal = peers.flatMap(a => a.wallet.privateKeyAccounts()).map(acc => app.consensusModule.generatingBalance(acc.address)).sum
    genBal should be >= (peers.head.transactionModule.InitialBalance / 4)
    genValidTransaction()

    waitGenerationOfBlocks(10)

    val last = peers.head.blockStorage.history.lastBlock
    untilTimeout(5.minutes, 10.seconds) {
      peers.head.blockStorage.history.contains(last) shouldBe true
    }
  }

  test("Generate block with plenty of transactions") {
    applications.tail.foreach { app =>
      app.wallet.privateKeyAccounts().foreach { acc =>
        if (state.asInstanceOf[BalanceSheet].balance(acc.address) > 0) {
          genValidTransaction(recepientOpt = accounts.headOption, senderOpt = Some(acc))
        }
      }
    }
    waitGenerationOfBlocks(1)

    val block = untilTimeout(3.minute) {
      stopGeneration()
      transactionModule.clearIncorrectTransactions()
      val toGen = transactionModule.utxStorage.SizeLimit - transactionModule.utxStorage.all().size
      (0 until toGen) foreach (i => genValidTransaction())
      val blocksFuture = application.consensusModule.generateNextBlocks(accounts)(transactionModule)
      val blocks: Seq[Block] = Await.result(blocksFuture, 10.seconds)
      blocks.nonEmpty shouldBe true
      blocks.head
    }

    block.isValid shouldBe true
    block.transactions.nonEmpty shouldBe true

    startGeneration()

  }


  test("Don't include same transactions twice") {
    //Wait until all peers contain transactions
    val (incl, h) = untilTimeout(1.minutes, 1.seconds) {
      val last = history.lastBlock
      val h = history.heightOf(last).get
      val incl = includedTransactions(last, history)
      require(incl.nonEmpty)
      peers.foreach { p =>
        incl foreach { tx =>
          p.blockStorage.state.included(tx).isDefined shouldBe true
          p.blockStorage.state.included(tx).get should be <= h
        }
      }
      (incl, h)
    }

    stopGeneration()
    cleanTransactionPool()

    incl.foreach(tx => transactionModule.utxStorage.putIfNew(tx))
    transactionModule.utxStorage.all().size shouldBe incl.size
    val tx = genValidTransaction(randomAmnt = false)
    transactionModule.utxStorage.all().size shouldBe incl.size + 1

    applications.foreach(_.blockGenerator ! StartGeneration)

    waitGenerationOfBlocks(2)

    peers.foreach { p =>
      incl foreach { tx =>
        p.blockStorage.state.included(tx).isDefined shouldBe true
        p.blockStorage.state.included(tx).get should be <= h
      }
    }
  }

  test("Double spending") {
    val recepient = new PublicKeyAccount(Array.empty)
    val (trans, valid) = untilTimeout(5.seconds) {
      cleanTransactionPool()
      stopGeneration()
      accounts.map(a => state.asInstanceOf[BalanceSheet].balance(a.address)).exists(_ > 2) shouldBe true
      val trans = accounts.flatMap { a =>
        val senderBalance = state.asInstanceOf[BalanceSheet].balance(a.address)
        (1 to 2) map (i => transactionModule.createPayment(a, recepient, senderBalance / 2, 1))
      }
      state.validate(trans).nonEmpty shouldBe true
      val valid = transactionModule.packUnconfirmed()
      valid.nonEmpty shouldBe true
      (trans, valid)
    }
    state.validate(trans).nonEmpty shouldBe true
    if (valid.size >= trans.size) {
      log.error(s"Double spending: $trans | $valid | ${state.asInstanceOf[BalanceSheet].balance(trans.head.sender.address)}")
    }
    valid.size should be < trans.size

    waitGenerationOfBlocks(2)

    accounts.foreach(a => state.asInstanceOf[BalanceSheet].balance(a.address) should be >= 0L)
    trans.exists(tx => state.included(tx).isDefined) shouldBe true // Some of transactions should be included in state
    trans.forall(tx => state.included(tx).isDefined) shouldBe false // But some should not
    startGeneration()
  }

  test("Rollback state") {
    def rollback(i: Int = 5) {

      val last = history.lastBlock
      val st1 = state.hash
      val height = history.heightOf(last).get

      //Wait for nonEmpty block
      untilTimeout(1.minute, 1.second) {
        genValidTransaction()
        peers.foreach(_.blockStorage.history.height() should be > height)
        history.height() should be > height
        history.lastBlock.transactions.nonEmpty shouldBe true
        state.hash should not be st1
        peers.foreach(_.transactionModule.blockStorage.history.contains(last))
      }
      waitGenerationOfBlocks(0)

      if (history.contains(last) || i < 0) {
        stopGeneration()
        peers.foreach { p =>
          p.transactionModule.blockStorage.removeAfter(last.uniqueId)
          p.history.lastBlock.encodedId shouldBe last.encodedId
        }
        state.hash shouldBe st1
        startGeneration()
      } else rollback(i - 1)
    }
    rollback()
  }

  def startGeneration(): Unit = {
    peers.foreach(_.blockGenerator ! StartGeneration)
  }

  def stopGeneration(): Unit = {
    log.info("Stop generation for all peers")
    peers.foreach(_.blockGenerator ! StopGeneration)
    untilTimeout(5.seconds) {
      peers.foreach { p =>
        Await.result(p.blockGenerator ? GetStatus, timeout.duration) shouldBe Syncing.name
      }
    }

  }
}