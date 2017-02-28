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

}
