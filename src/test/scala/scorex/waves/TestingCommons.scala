package scorex.waves

import java.util.concurrent.atomic.AtomicInteger

import com.ning.http.client.Response
import com.wavesplatform.{Application, ChainParameters, TestNetParams}
import dispatch.{Http, url}
import play.api.libs.json.{JsObject, JsValue, Json}
import scorex.account.{Account, AddressScheme}
import scorex.transaction.{GenesisTransaction, TransactionSettings}
import scorex.utils._
import com.wavesplatform.settings.{Constants, WavesSettings}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor.ActorSystem
import scorex.settings.Settings

trait TestingCommons {

  implicit object TestTransactionLayerSettings extends TransactionSettings {
    override val settingsJSON: JsObject = Json.obj()
  }

}

object UnitTestNetParams extends ChainParameters {
  val initialBalance = Constants.UnitsInWave * Constants.TotalWaves
  val genesisTimestamp = 1478000000000L
  val singleNodeBalance = initialBalance * 0.02
  val genesisTxs = {
    val txs = Seq(
      GenesisTransaction(new Account("3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K"), (2 * singleNodeBalance).toLong, genesisTimestamp),
      GenesisTransaction(new Account("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8"), singleNodeBalance.toLong, genesisTimestamp),
      GenesisTransaction(new Account("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh"), singleNodeBalance.toLong, genesisTimestamp),
      GenesisTransaction(new Account("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd"), singleNodeBalance.toLong, genesisTimestamp),
      GenesisTransaction(new Account("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx"), (initialBalance - 5 * singleNodeBalance).toLong, genesisTimestamp)
    )
    require(txs.foldLeft(0L)(_ + _.amount) == initialBalance)
    txs
  }
  override val addressScheme: AddressScheme = new AddressScheme {
    override val chainId: Byte = 'T'.toByte
  }

  override val allowTemporaryNegativeUntil: Long = 1477958400000L

  override val requireSortedTransactionsAfter: Long = 1477958400000L

  override val allowInvalidPaymentTransactionsByTimestamp: Long = 1477958400000L

  override val generatingBalanceDepthFrom50To1000AfterHeight: Long = Long.MinValue

  override def minimalGeneratingBalanceAfterTimestamp: Long = Long.MinValue

  override def allowTransactionsFromFutureUntil: Long = Long.MinValue

  override def allowUnissuedAssetsUntil: Long = 1479416400000L

  override def allowDeleteTransactionAfterTimestamp: Long = 1481110521000L
}

object TestingCommons {

  AddressScheme.current = TestNetParams.addressScheme
  lazy val applications = {
    val apps = List(
      new Application(ActorSystem("test"), new WavesSettings(Settings.readSettingsJson("settings-test.json")) {
        override lazy val chainParams = UnitTestNetParams
        override lazy val walletDirOpt = None
        override lazy val dataDirOpt = None
      })
    )
    apps.foreach(_.run())
    apps.foreach { a =>
      if (a.wallet.privateKeyAccounts().isEmpty) a.wallet.generateNewAccounts(3)
      untilTimeout(20.seconds, 1.second) {
        val request = Http(url(peerUrl(a) + "/consensus/algo").GET)
        val response = Await.result(request, 10.seconds)
        val json = Json.parse(response.getResponseBody).as[JsObject]
        assert((json \ "consensusAlgo").asOpt[String].isDefined)
      }
    }
    apps
  }

  lazy val application = applications.head

  lazy val counter: AtomicInteger = new AtomicInteger(0)

  def start(): Unit = {
    counter.incrementAndGet
  }

  def stop(): Unit = {
    if (counter.decrementAndGet == 0) {
      Http.shutdown()
      application.shutdown()
    }
  }

  def peerUrl(a: Application = application): String =
    "http://" + a.settings.bindAddress + ":" + a.settings.rpcPort

  def matcherUrl(a: Application = application): String =
    "http://" + a.settings.matcherHost + ":" + a.settings.matcherPort + "/matcher"

  def getRequest(us: String, peer: String = peerUrl(application),
                 headers: Map[String, String] = Map.empty): JsValue = {
    val request = Http(url(peer + us).GET <:< headers)
    val response = Await.result(request, 10.seconds)
    Json.parse(response.getResponseBody)
  }

  def matcherGetRequest(path: String, params: Map[String, String] = Map.empty): JsValue = {
    val request = Http(url(matcherUrl() + path).GET <<? params)
    val response = Await.result(request, 10.seconds)
    Json.parse(response.getResponseBody)
  }


  def postRequest(us: String,
                  params: Map[String, String] = Map.empty,
                  body: String = "",
                  headers: Map[String, String] = Map("api_key" -> "test"),
                  peer: String = peerUrl(application)): JsValue = {
    val response = postRequestWithResponse(us, params, body, headers, peer)
    Json.parse(response.getResponseBody)
  }

  def matcherPostRequest(path: String, body: String = "",
                  headers: Map[String, String] = Map("api_key" -> "test")): JsValue = {
    val request = Http(url(matcherUrl() + path).POST <:< headers << body )
    val response = Await.result(request, 5.seconds)
    Json.parse(response.getResponseBody)
  }

  def postRequestWithResponse(us: String,
                              params: Map[String, String] = Map.empty,
                              body: String = "",
                              headers: Map[String, String] = Map("api_key" -> "test"),
                              peer: String = peerUrl(application)): Response = {
    val request = Http(url(peer + us).POST << params <:< headers << body)
    Await.result(request, 5.seconds)
  }
}
