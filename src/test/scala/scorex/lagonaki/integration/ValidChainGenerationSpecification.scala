package scorex.lagonaki.integration

import akka.pattern.ask
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scorex.account.PublicKeyAccount
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController._
import scorex.lagonaki.{TestingCommons, TransactionTestingCommons}
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.{BalanceSheet, SimpleTransactionModule}
import scorex.utils.{ScorexLogging, untilTimeout}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

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
    untilTimeout(5.minutes, 10.seconds) {
      val heights = peers.map(_.blockStorage.history.height())
      log.info(s"Current heights are: $heights. Waiting for ${height + howMany}")
      heights.foreach(_ should be >= height + howMany)
    }
  }

  def maxHeight(): Int = peers.map(_.blockStorage.history.height()).max

  def cleanTransactionPool(): Unit = untilTimeout(1.second) {
    UnconfirmedTransactionsDatabaseImpl.all().foreach(tx => UnconfirmedTransactionsDatabaseImpl.remove(tx))
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe 0
  }

  test("generate 10 blocks and synchronize") {
    val genBal = peers.flatMap(a => a.wallet.privateKeyAccounts()).map(app.blockStorage.state.generationBalance(_)).sum
    genBal should be >= (peers.head.transactionModule.InitialBalance / 4)
    genValidTransaction()

    waitGenerationOfBlocks(10)

    val last = peers.head.blockStorage.history.lastBlock
    untilTimeout(5.minutes, 10.seconds) {
      peers.head.blockStorage.history.contains(last) shouldBe true
    }
  }

  test("Generate block with plenty of transactions") {
    val block = untilTimeout(1.minute) {
      stopGeneration()
      if (transactionModule.packUnconfirmed().size < UnconfirmedTransactionsDatabaseImpl.SizeLimit) {
        (0 to UnconfirmedTransactionsDatabaseImpl.SizeLimit) foreach (i => genValidTransaction())

      }
      val blocksFuture = application.consensusModule.generateNextBlocks(Seq(accounts.head))(transactionModule)
      val blocks: Seq[Block] = Await.result(blocksFuture, 10.seconds)
      blocks.nonEmpty shouldBe true
      blocks.head
    }

    block.isValid shouldBe true
    block.transactions.nonEmpty shouldBe true

    startGeneration()

  }


  test("Don't include same transactions twice") {
    val last = history.lastBlock
    val h = history.heightOf(last).get
    val incl = includedTransactions(last, history)
    require(incl.nonEmpty)
    waitGenerationOfBlocks(0) // all peer should contain common block
    peers.foreach { p =>
      incl foreach { tx =>
        p.blockStorage.state.included(tx).isDefined shouldBe true
        p.blockStorage.state.included(tx).get should be <= h
      }
    }

    stopGeneration()
    cleanTransactionPool()

    incl.foreach(tx => UnconfirmedTransactionsDatabaseImpl.putIfNew(tx))
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe incl.size
    val tx = genValidTransaction(randomAmnt = false)
    UnconfirmedTransactionsDatabaseImpl.all().size shouldBe incl.size + 1

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
      val trans = accounts.flatMap { a =>
        val senderBalance = state.asInstanceOf[BalanceSheet].balance(a.address)
        (1 to 2) map (i => transactionModule.createPayment(a, recepient, senderBalance / 2, 1))
      }
      val valid = transactionModule.packUnconfirmed()
      valid.nonEmpty shouldBe true
      (trans, valid)
    }
    state.validate(trans).nonEmpty shouldBe true
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