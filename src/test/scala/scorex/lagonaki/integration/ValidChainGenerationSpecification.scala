package scorex.lagonaki.integration

import akka.pattern.ask
import com.wavesplatform.Application
import com.wavesplatform.settings.Constants
import org.scalatest.concurrent.Eventually
import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.{JsArray, JsNumber, JsString}
import scorex.consensus.nxt.{WavesConsensusModule}
import scorex.lagonaki.TransactionTestingCommons
import scorex.network.peer.PeerManager.GetBlacklistedPeers
import scorex.utils.untilTimeout

import scala.concurrent.Await
import scala.concurrent.duration._

class ValidChainGenerationSpecification extends FunSuite with Matchers with Eventually with TransactionTestingCommons {

  override def beforeAll(): Unit = {
    super.beforeAll()
    waitForSingleConnection(application)
    Thread.sleep(1000)
  }

  def cleanTransactionPool(): Unit = untilTimeout(1.second) {
    transactionModule.utxStorage.all().foreach(tx => transactionModule.utxStorage.remove(tx))
    transactionModule.utxStorage.all().size shouldBe 0
  }

  private def getMaxWalletGeneratingBalanceWaves(app: Application): Long = {
    val url = peerUrl(app)
    val addressesResponse = GET.request("/addresses", peer = url)
    val addresses = addressesResponse.as[List[String]]

    addresses.map(address => {
      val balanceResponse = GET.request(s"/consensus/generatingbalance/$address", peer = url)
      (balanceResponse \ "balance").asOpt[JsNumber].get.value.toLongExact / Constants.UnitsInWave
    }).max
  }

  private def getHeight(app: Application): Int = {
    val url = peerUrl(app)
    val response = GET.request("/blocks/height", peer = url)

    (response \ "height").asOpt[JsNumber].get.value.toIntExact
  }

  private def getBlockIdAtHeight(app: Application, height: Int): String = {
    val url = peerUrl(app)
    val response = GET.request(s"/blocks/at/$height", peer = url)

    (response \ "signature").asOpt[JsString].get.value
  }

  private def blacklistedPeersFor(app: Application) =
    Await.result((app.peerManager ? GetBlacklistedPeers).mapTo[Set[String]], timeout.duration)

  private def checkBlacklists() = applications.foreach { app => assert(blacklistedPeersFor(app).isEmpty) }

  test("generate 30 blocks and synchronize") {
    // Check that all 3 nodes are connected at least to 2 other nodes
    eventually(timeout(30.seconds), interval(250.millis)) {
      applications.foreach(getConnectedPeersCount(_) should be > 1)
    }

    // Check that every node has enough money to generate blocks
    val requiredBalanceForGenerationInWaves =
      WavesConsensusModule.MinimalEffectiveBalanceForGenerator / Constants.UnitsInWave

    eventually(timeout(1.second), interval(100.millis)) {
      applications.exists(getMaxWalletGeneratingBalanceWaves(_) > requiredBalanceForGenerationInWaves) shouldBe true
    }

    // Check that all nodes have the same genesis block
    val firstBlock = getBlockIdAtHeight(application, 1)
    eventually(timeout(1.minutes), interval(500.millis)) {
      applications.forall(a => {
        getBlockIdAtHeight(a, 1) == firstBlock
      }) shouldBe true
    }

    genValidTransaction()

    // Run nodes and wait for all of them have at least 30 blocks in their blockchains
    eventually(timeout(3.minutes), interval(2.seconds)) {
      applications.forall(a => {
        val h = getHeight(a)
        getBlockIdAtHeight(a, h)

        h > 30
      }) shouldBe true
    }

    // Check that nodes are in sync
    val expectedHeight = 30
    val expectedBlock = getBlockIdAtHeight(application, expectedHeight)
    eventually(timeout(1.minutes), interval(100.millis)) {
      applications.forall(a => {
        val block = getBlockIdAtHeight(a, expectedHeight)
        block == expectedBlock
      }) shouldBe true
    }

    // Check that nodes are continue to sync
    val lastHeight = getHeight(application)
    val lastBlock = getBlockIdAtHeight(application, lastHeight)
    eventually(timeout(1.minutes), interval(100.millis)) {
      applications.forall(a => {
        val block = getBlockIdAtHeight(a, lastHeight)
        block == lastBlock
      }) shouldBe true
    }
  }


  /*
    ignore("Generate block with plenty of transactions") {
      applications.tail.foreach { app =>
        app.wallet.privateKeyAccounts().foreach { acc =>
          if (state.asInstanceOf[BalanceSheet].balance(acc) > 0) {
            genValidTransaction(recipientOpt = applicationNonEmptyAccounts.headOption, senderOpt = Some(acc))
          }
        }
      }
      waitGenerationOfBlocks(1)

      val block = untilTimeout(3.minute) {
        stopGeneration(applications)
        transactionModule.clearIncorrectTransactions()
        val toGen = transactionModule.utxStorage.sizeLimit - transactionModule.utxStorage.all().size
        (0 until toGen) foreach (i => genValidTransaction())
        val blocks = application.consensusModule.generateNextBlocks(applicationNonEmptyAccounts)(transactionModule)
        blocks.nonEmpty shouldBe true
        blocks.head
      }

      block.isValid shouldBe true
      block.transactions.nonEmpty shouldBe true

      startGeneration(applications)
    }

    ignore("Don't include same transactions twice") {
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

      stopGeneration(applications)
      cleanTransactionPool()

      incl.foreach(tx => transactionModule.utxStorage.putIfNew(tx))
      transactionModule.utxStorage.all().size shouldBe incl.size
      val tx = genValidTransaction(randomAmnt = false)
      transactionModule.utxStorage.all().size shouldBe incl.size + 1

      startGeneration(applications)

      waitGenerationOfBlocks(2)

      peers.foreach { p =>
        incl foreach { tx =>
          p.blockStorage.state.included(tx).isDefined shouldBe true
          p.blockStorage.state.included(tx).get should be <= h
        }
      }
    }

    ignore("Double spending") {
      val recepient = new PublicKeyAccount(Array.empty)
      val (trans, valid) = untilTimeout(5.seconds) {
        cleanTransactionPool()
        stopGeneration(applications)
        applicationNonEmptyAccounts.map(a => state.asInstanceOf[BalanceSheet].balance(a)).exists(_ > 2) shouldBe true
        val trans = applicationNonEmptyAccounts.flatMap { a =>
          val senderBalance = state.asInstanceOf[BalanceSheet].balance(a)
          (1 to 2) map (i => transactionModule.createPayment(a, recepient, senderBalance / 2, 1).right.get)
        }
        state.validate(trans, blockTime = trans.map(_.timestamp).max).nonEmpty shouldBe true
        val valid = transactionModule.packUnconfirmed()
        valid.nonEmpty shouldBe true
        (trans, valid)
      }
      state.validate(trans, blockTime = trans.map(_.timestamp).max).nonEmpty shouldBe true
      if (valid.size >= trans.size) {
        val balance = state.asInstanceOf[BalanceSheet].balance(trans.head.sender)
      }
      valid.size should be < trans.size

      waitGenerationOfBlocks(2)

      applicationNonEmptyAccounts.foreach(a => state.asInstanceOf[BalanceSheet].balance(a) should be >= 0L)
      trans.exists(tx => state.included(tx).isDefined) shouldBe true // Some of transactions should be included in state
      trans.forall(tx => state.included(tx).isDefined) shouldBe false // But some should not

      startGeneration(applications)
    }

    ignore("Rollback state") {
      def rollback(i: Int = 5) {

        val last = history.lastBlock
        val st1 = state.hash
        val height = history.heightOf(last).get
        val recepient = application.wallet.generateNewAccount()

        //Wait for nonEmpty block
        untilTimeout(1.minute, 1.second) {
          genValidTransaction(recipientOpt = recepient)
          peers.foreach(_.blockStorage.history.height() should be > height)
          history.height() should be > height
          history.lastBlock.transactions.nonEmpty shouldBe true
          peers.foreach(_.transactionModule.blockStorage.history.contains(last))
        }
        state.hash should not be st1
        waitGenerationOfBlocks(0)

        if (peers.forall(p => p.history.contains(last))) {
          stopGeneration(applications)
          peers.foreach { p =>
            p.transactionModule.blockStorage.removeAfter(last.uniqueId)
          }
          peers.foreach { p =>
            p.transactionModule.blockStorage.removeAfter(last.uniqueId)
            p.history.lastBlock.encodedId shouldBe last.encodedId
          }
          state.hash shouldBe st1
          startGeneration(applications)
        } else {
          require(i > 0, "History should contain last block at least sometimes")
          rollback(i - 1)
        }
      }

      rollback()
    }
  */
}
