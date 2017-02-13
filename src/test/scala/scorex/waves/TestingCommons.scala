package scorex.waves

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.ning.http.client.Response
import com.typesafe.config.ConfigFactory
import com.wavesplatform.Application
import com.wavesplatform.settings.{Constants, WavesSettings}
import dispatch.{Http, url}
import org.scalatest.{BeforeAndAfterAll, Suite}
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import scorex.account.{Account, AddressScheme}
import scorex.api.http.ApiKeyNotValid
import scorex.consensus.mining.BlockGeneratorController.{GetBlockGenerationStatus, Idle, StartGeneration, StopGeneration}
import scorex.settings.ChainParameters
import scorex.transaction.{GenesisTransaction, Transaction}
import scorex.utils._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random


object UnitTestNetParams extends ChainParameters {
  val initialBalance: Long = Constants.UnitsInWave * Constants.TotalWaves
  val genesisTimestamp = 1478000000000L
  val singleNodeBalance: Double = initialBalance * 0.02
  lazy val genesisTxs: Seq[Transaction] = {
    val txs = Seq(
      GenesisTransaction.create(new Account("3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K"), (2 * singleNodeBalance).toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd"), singleNodeBalance.toLong, genesisTimestamp).right.get,
      GenesisTransaction.create(new Account("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx"), (initialBalance - 5 * singleNodeBalance).toLong, genesisTimestamp).right.get
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

  override def allowBurnTransactionAfterTimestamp: Long = 1481110521000L

  override def requirePaymentUniqueId: Long = 0L
}

trait TestingCommons extends Suite with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    stop()
  }

  /*
    implicit object TestTransactionLayerSettings extends Settings {
      override val settingsJSON: JsObject = Json.obj()
    }
  */

  def randomFrom[T](seq: Seq[T]): T = {
    require(seq.nonEmpty)
    seq(Random.nextInt(seq.length))
  }

  implicit val timeout = Timeout(1.second)

  private val config1 = ConfigFactory.parseString(
    """
      |waves {
      |  miner {
      |    enable: yes
      |    offline: yes
      |    quorum: 1
      |    block-generation-delay: 1s
      |    interval-after-last-block-then-generation-is-allowed: 10m
      |    tf-like-scheduling: yes
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()
  val wavesSettings1: WavesSettings = WavesSettings.fromConfig(config1)

  private val config2 = ConfigFactory.parseString(
    """
      |waves {
      |  miner {
      |    enable: yes
      |    offline: yes
      |    quorum: 1
      |    block-generation-delay: 1s
      |    interval-after-last-block-then-generation-is-allowed: 10m
      |    tf-like-scheduling: yes
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()
  val wavesSettings2: WavesSettings = WavesSettings.fromConfig(config2)

  private val config3 = ConfigFactory.parseString(
    """
      |waves {
      |  miner {
      |    enable: yes
      |    offline: yes
      |    quorum: 1
      |    block-generation-delay: 1s
      |    interval-after-last-block-then-generation-is-allowed: 10m
      |    tf-like-scheduling: yes
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()
  val wavesSettings3: WavesSettings = WavesSettings.fromConfig(config3)


  val applications = List(
    new Application(ActorSystem("test0"), WavesSettings.fromConfig(config1)) /* {
      override lazy val chainParams = UnitTestNetParams
      override lazy val walletDirOpt = None
      override lazy val dataDirOpt = None
      override lazy val nodeNonce = 111L
    })*/ ,
    new Application(ActorSystem("test1"), WavesSettings.fromConfig(config2)) /*Settings.readSettingsJson("settings-local1.json")) {
      override lazy val chainParams = UnitTestNetParams
      override lazy val walletDirOpt = None
      override lazy val dataDirOpt = None
      override lazy val nodeNonce = 222L
    })*/ ,
    new Application(ActorSystem("test2"), WavesSettings.fromConfig(config3)) /*Settings.readSettingsJson("settings-local2.json")) {
      override lazy val chainParams = UnitTestNetParams
      override lazy val walletDirOpt = None
      override lazy val dataDirOpt = None
      override lazy val nodeNonce = 333L
    })*/
  )
  applications.foreach(_.run())
  applications.foreach { a =>
    if (a.wallet.privateKeyAccounts().isEmpty) a.wallet.generateNewAccounts(3)
    untilTimeout(20.seconds, 1.second) {
      val request = Http(url(peerUrl(a) + "/consensus/algo").GET)
      val response = Await.result(request, 10.seconds)
      val json = Json.parse(response.getResponseBody).as[JsObject]
      assert((json \ "consensusAlgo").asOpt[String].isDefined)
    }
  }


  val application: Application = applications.head

  def stop(): Unit = {
    applications.foreach(a => {

      if (a.settings.matcherSettings.enable) {
        a.shutdownMatcher()
      }
      a.shutdown()
    })
  }

  def waitForNextBlock(application: Application): Unit = {
    val history = application.transactionModule.blockStorage.history
    val initialHeight = history.height()
    untilTimeout(15.seconds) {
      require(history.height() > initialHeight)
    }
  }

  def getConnectedPeersCount(app: Application): Int = {
    val response = GET.request("/peers/connected", peer = peerUrl(app))
    val peers = (response \ "peers").asOpt[JsArray]

    peers.get.value.size
  }

  def waitForSingleConnection(application: Application): Unit = {
    untilTimeout(30.seconds) {
      require(getConnectedPeersCount(application) > 0)
    }
  }

  def forgeSignature(signature: Array[Byte]): Array[Byte] = {
    val modifier: BigInt = BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")
    signature.take(32) ++ (BigInt(signature.takeRight(32).reverse) + modifier).toByteArray.reverse
  }

  def peerUrl(a: Application = application): String =
    "http://" + a.settings.restAPISettings.bindAddress + ":" + a.settings.restAPISettings.port

  def matcherUrl(a: Application = application): String =
    "http://" + a.settings.restAPISettings.bindAddress + ":" + a.settings.restAPISettings.port + "/matcher"

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
                         params: Map[String, String] = Map.empty,
                         headers: Map[String, String] = Map("api_key" -> "test")): JsValue = {
    val request = Http(url(matcherUrl() + path).POST <:< headers <<? params << body)
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

  def startGeneration(nodes: Seq[Application]): Unit = {
    nodes.foreach(_.blockGenerator ! StartGeneration)
  }

  def stopGeneration(nodes: Seq[Application]): Unit = {
    nodes.foreach(_.blockGenerator ! StopGeneration)
    untilTimeout(5.seconds) {
      nodes.foreach { p =>
        require(Await.result(p.blockGenerator ? GetBlockGenerationStatus, timeout.duration) == Idle.name)
      }
    }
  }

  sealed trait RequestType {
    def incorrectApiKeyTest(path: String): Unit = {
      Seq(Map[String, String](), Map("api_key" -> "wrong key")) foreach { h =>
        val resp = request(path, headers = h).toString()
        assert(resp == ApiKeyNotValid.json.toString(), s"$resp == ${ApiKeyNotValid.json.toString()} is false")
      }
    }

    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map("api_key" -> "test"),
                peer: String = peerUrl(application)): JsValue

    def requestRaw(us: String,
                   params: Map[String, String] = Map.empty,
                   body: String = "",
                   headers: Map[String, String] = Map("api_key" -> "test"),
                   peer: String = peerUrl(application)): Response
  }

  case object GET extends RequestType {
    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map.empty,
                peer: String = peerUrl(application)): JsValue = {
      val request = Http(url(peer + us).GET <:< headers)
      val response: Response = Await.result(request, 5.seconds)
      Json.parse(response.getResponseBody)
    }

    override def requestRaw(us: String, params: Map[String, String], body: String, headers: Map[String, String], peer: String): Response = {
      val request = Http(url(peer + us).GET <:< headers)
      Await.result(request, 5.seconds)
    }
  }

  case object POST extends RequestType {
    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map("api_key" -> "test"),
                peer: String = peerUrl(application)): JsValue = {
      val request = Http(url(peer + us).POST << params <:< headers << body)
      val response = Await.result(request, 5.seconds)
      Json.parse(response.getResponseBody)
    }

    override def requestRaw(us: String, params: Map[String, String], body: String, headers: Map[String, String], peer: String): Response = {
      val request = Http(url(peer + us).POST << params <:< headers << body)
      Await.result(request, 5.seconds)
    }
  }

  case object DELETE extends RequestType {
    def request(us: String,
                params: Map[String, String] = Map.empty,
                body: String = "",
                headers: Map[String, String] = Map("api_key" -> "test"),
                peer: String = peerUrl(application)): JsValue = {
      val request = Http(url(peer + us).DELETE << params <:< headers << body)
      val response = Await.result(request, 5.seconds)
      Json.parse(response.getResponseBody)
    }

    override def requestRaw(us: String, params: Map[String, String], body: String, headers: Map[String, String], peer: String): Response = {
      val request = Http(url(peer + us).DELETE << params <:< headers << body)
      Await.result(request, 5.seconds)
    }
  }

}
