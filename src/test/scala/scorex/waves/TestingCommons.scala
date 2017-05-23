package scorex.waves

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import com.ning.http.client.Response
import com.typesafe.config.ConfigFactory
import com.wavesplatform.Application
import com.wavesplatform.settings.WavesSettings
import dispatch.{Http, Req, url}
import org.scalatest.concurrent.Eventually
import org.scalatest.{BeforeAndAfterAll, Suite}
import play.api.libs.json._
import scorex.account.Account
import scorex.api.http.ApiKeyNotValid
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.consensus.mining.BlockGeneratorController.{GetBlockGenerationStatus, Idle, StartGeneration, StopGeneration}
import scorex.crypto.encode.Base58
import scorex.transaction.AssetAcc
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

trait TestingCommons extends Suite with BeforeAndAfterAll with Eventually {

  override def beforeAll(): Unit = {
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    stop()
  }

  def randomFrom[T](seq: Seq[T]): T = {
    require(seq.nonEmpty)
    seq(Random.nextInt(seq.length))
  }

  implicit val timeout = Timeout(1.second)

  private val config1 = ConfigFactory.parseString(
    """
      |waves {
      |  directory: ""
      |  logging-level: DEBUG
      |  network {
      |    file: ""
      |    bind-address: "127.0.0.1"
      |    port: 9091
      |    node-name: "Node-1"
      |    known-peers = ["127.0.0.1:9088","127.0.0.1:9084"]
      |    local-only: yes
      |    max-connections: 10
      |    peers-broadcast-interval: 500
      |    upnp {
      |      enable: no
      |    }
      |  }
      |  wallet {
      |    file: ""
      |    password: "cookies"
      |    seed: "FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"
      |  }
      |  blockchain {
      |    file: ""
      |    type: CUSTOM
      |    custom {
      |      address-scheme-character: "T"
      |      functionality {
      |        allow-temporary-negative-until: 1477958400000
      |        allow-invalid-payment-transactions-by-timestamp: 1477958400000
      |        require-sorted-transactions-after: 1477958400000
      |        generation-balance-depth-from-50-to-1000-after-height: 0
      |        minimal-generating-balance-after: 0
      |        allow-transactions-from-future-until: 0
      |        allow-unissued-assets-until: 1479416400000
      |        allow-burn-transaction-after: 1481110521000
      |        allow-lease-transaction-after: 0
      |        require-payment-unique-id-after: 0
      |        allow-exchange-transaction-after: 0
      |        allow-invalid-reissue-in-same-block-until-timestamp: 0
      |        allow-multiple-lease-cancel-transaction-until-timestamp: 0
      |        reset-effective-balances-at-height: 0
      |        allow-leased-balance-transfer-until: 1477958400000
      |      }
      |      genesis {
      |        timestamp: 1478000000000
      |        signature: "s1ohWATVbaej8m8wKC9jec5NjZTGR3f4DUtSUoVnmSBwetPcfwYrBLFuWM4bnRXa8gJKcWgAzdyie6fqxLUuDiY"
      |        initial-balance: 10000000000000000
      |        transactions = [
      |          {recipient: "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K", amount: 400000000000000}
      |          {recipient: "3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", amount: 200000000000000}
      |          {recipient: "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", amount: 200000000000000}
      |          {recipient: "3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", amount: 200000000000000}
      |          {recipient: "3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", amount: 9000000000000000}
      |        ]
      |      }
      |    }
      |  }
      |  checkpoints {
      |    public-key: ""
      |  }
      |  fees {
      |    payment {
      |      WAVES = 0
      |    }
      |    issue {
      |      WAVES = 10000000
      |    }
      |    transfer {
      |      WAVES = 100000
      |    }
      |    reissue {
      |      WAVES = 100000
      |    }
      |    burn {
      |      WAVES = 100000
      |    }
      |    exchange {
      |      WAVES = 100000
      |    }
      |  }
      |  matcher {
      |    enable: yes
      |    account: "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K"
      |    port: 9093
      |    journal-directory: ${waves.directory}"/journal"
      |    snapshots-directory: ${waves.directory}"/snapshots"
      |    order-history-file = ""
      |    max-timestamp-diff = 3h
      |    is-migrate-to-new-order-history-storage = no
      |  }
      |  miner {
      |    enable: yes
      |    generation-delay: 500
      |    tf-like-scheduling: no
      |  }
      |  rest-api {
      |    enable: yes
      |    port: 9092
      |    api-key-hash: "JDJkZrg24XwvBgBUi6PgpHzrAFgeefb7nU8LJPRR58ga"
      |  }
      |  synchronization {
      |    score-broadcast-interval: 500
      |  }
      |  utx {
      |    size: 1000
      |    broadcast-interval: 500
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()
  val wavesSettings1: WavesSettings = WavesSettings.fromConfig(config1)

  private val config2 = ConfigFactory.parseString(
    """
      |waves {
      |  directory: ""
      |  logging-level: DEBUG
      |  network {
      |    file: ""
      |    bind-address: "127.0.0.1"
      |    port: 8084
      |    node-name: "Node-2"
      |    known-peers = ["127.0.0.1:9088","127.0.0.1:9091"]
      |    local-only: yes
      |    max-connections: 10
      |    peers-broadcast-interval: 900
      |    upnp {
      |      enable: no
      |    }
      |  }
      |  wallet {
      |    file: ""
      |    password: "cookies"
      |    seed: "111"
      |  }
      |  blockchain {
      |    file: ""
      |    type: CUSTOM
      |    custom {
      |      address-scheme-character: "T"
      |      functionality {
      |        allow-temporary-negative-until: 1477958400000
      |        allow-invalid-payment-transactions-by-timestamp: 1477958400000
      |        require-sorted-transactions-after: 1477958400000
      |        generation-balance-depth-from-50-to-1000-after-height: 0
      |        minimal-generating-balance-after: 0
      |        allow-transactions-from-future-until: 0
      |        allow-unissued-assets-until: 1479416400000
      |        allow-burn-transaction-after: 1481110521000
      |        allow-lease-transaction-after: 0
      |        require-payment-unique-id-after: 0
      |        allow-exchange-transaction-after: 0
      |        allow-invalid-reissue-in-same-block-until-timestamp: 0
      |        allow-multiple-lease-cancel-transaction-until-timestamp: 0
      |        reset-effective-balances-at-height: 0
      |        allow-leased-balance-transfer-until: 1477958400000
      |      }
      |      genesis {
      |        timestamp: 1478000000000
      |        signature: "s1ohWATVbaej8m8wKC9jec5NjZTGR3f4DUtSUoVnmSBwetPcfwYrBLFuWM4bnRXa8gJKcWgAzdyie6fqxLUuDiY"
      |        initial-balance: 10000000000000000
      |        transactions = [
      |          {recipient: "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K", amount: 400000000000000}
      |          {recipient: "3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", amount: 200000000000000}
      |          {recipient: "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", amount: 200000000000000}
      |          {recipient: "3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", amount: 200000000000000}
      |          {recipient: "3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", amount: 9000000000000000}
      |        ]
      |      }
      |    }
      |  }
      |  checkpoints {
      |    public-key: ""
      |  }
      |  matcher {
      |    enable: false
      |  }
      |  miner {
      |    enable: yes
      |    generation-delay: 500
      |  }
      |  rest-api {
      |    enable: yes
      |    port: 9085
      |    api-key-hash: "EfwB3nNEwDSc885diz76v3mnPw2EhjyUSnDfw1XPbz92"
      |  }
      |  synchronization {
      |    score-broadcast-interval: 500
      |  }
      |  utx {
      |    size: 1234
      |    broadcast-interval: 500
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()
  val wavesSettings2: WavesSettings = WavesSettings.fromConfig(config2)

  private val config3 = ConfigFactory.parseString(
    """
      |waves {
      |  directory: ""
      |  logging-level: DEBUG
      |  network {
      |    file: ""
      |    bind-address: "127.0.0.1"
      |    port: 9088
      |    node-name: "Node-3"
      |    known-peers = ["127.0.0.1:9084","127.0.0.1:9091"]
      |    local-only: yes
      |    max-connections: 10
      |    peers-broadcast-interval: 900
      |    upnp {
      |      enable: no
      |    }
      |  }
      |  wallet {
      |    file: ""
      |    password: "cookies"
      |    seed: "222"
      |  }
      |  blockchain {
      |    file: ""
      |    type: CUSTOM
      |    custom {
      |      address-scheme-character: "T"
      |      functionality {
      |        allow-temporary-negative-until: 1477958400000
      |        allow-invalid-payment-transactions-by-timestamp: 1477958400000
      |        require-sorted-transactions-after: 1477958400000
      |        generation-balance-depth-from-50-to-1000-after-height: 0
      |        minimal-generating-balance-after: 0
      |        allow-transactions-from-future-until: 0
      |        allow-unissued-assets-until: 1479416400000
      |        allow-burn-transaction-after: 1481110521000
      |        allow-lease-transaction-after: 0
      |        require-payment-unique-id-after: 0
      |        allow-exchange-transaction-after: 0
      |        allow-invalid-reissue-in-same-block-until-timestamp: 0
      |        allow-multiple-lease-cancel-transaction-until-timestamp: 0
      |        reset-effective-balances-at-height: 0
      |        allow-leased-balance-transfer-until: 1477958400000
      |      }
      |      genesis {
      |        timestamp: 1478000000000
      |        signature: "s1ohWATVbaej8m8wKC9jec5NjZTGR3f4DUtSUoVnmSBwetPcfwYrBLFuWM4bnRXa8gJKcWgAzdyie6fqxLUuDiY"
      |        initial-balance: 10000000000000000
      |        transactions = [
      |          {recipient: "3N3keodUiS8WLEw9W4BKDNxgNdUpwSnpb3K", amount: 400000000000000}
      |          {recipient: "3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", amount: 200000000000000}
      |          {recipient: "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", amount: 200000000000000}
      |          {recipient: "3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", amount: 200000000000000}
      |          {recipient: "3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", amount: 9000000000000000}
      |        ]
      |      }
      |    }
      |  }
      |  checkpoints {
      |    public-key: ""
      |  }
      |  matcher {
      |    enable: no
      |  }
      |  miner {
      |    enable: yes
      |    generation-delay: 500
      |  }
      |  rest-api {
      |    enable: yes
      |    port: 9086
      |    api-key-hash: "EfwB3nNEwDSc885diz76v3mnPw2EhjyUSnDfw1XPbz92"
      |  }
      |  synchronization {
      |    score-broadcast-interval: 500
      |  }
      |  utx {
      |    size: 1234
      |    broadcast-interval: 500
      |  }
      |}
    """.stripMargin).withFallback(ConfigFactory.load()).resolve()
  val wavesSettings3: WavesSettings = WavesSettings.fromConfig(config3)


  val applications = List(
    new Application(ActorSystem("test0"), WavesSettings.fromConfig(config1)),
    new Application(ActorSystem("test1"), WavesSettings.fromConfig(config2)),
    new Application(ActorSystem("test2"), WavesSettings.fromConfig(config3)))

  applications.foreach(_.run())
  applications.foreach { a =>
    if (a.wallet.privateKeyAccounts().isEmpty) a.wallet.generateNewAccounts(3)
    untilTimeout(30.seconds, 3.second) {
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
    untilTimeout(15.seconds, 1.second) {
      require(history.height() > initialHeight)
    }
  }

  def getConnectedPeersCount(app: Application): Int = {
    val response = GET.requestJson("/peers/connected", peer = peerUrl(app))
    val peers = (response \ "peers").asOpt[JsArray]

    peers.get.value.size
  }

  def waitForSingleConnection(application: Application): Unit = {
    untilTimeout(30.seconds, 1.second) {
      require(getConnectedPeersCount(application) > 0)
    }
  }

  def assetTransfer(from: Account, to: Account, amount: Long, fee: Long = 100000L, assetId: Option[String] = None, feeAssetId: Option[String] = None, assertSuccess: Boolean = true): Option[String] = {
    val json = Json.toJson(TransferRequest(assetId, feeAssetId, amount, fee, from.address, None, to.address)).toString()
    makeTxRequest(POST.requestRaw(us = "/assets/transfer", body = json), assertSuccess)
  }

  def payment(from: Account, to: Account, amount: Long, fee: Long = 100000L, assetId: Option[String] = None, assertSuccess: Boolean = true): Option[String] = {
    val json = Json.toJson(PaymentRequest(amount, fee, from.address, to.address)).toString()
    makeTxRequest(POST.requestRaw(us = "/waves/payment", body = json), assertSuccess, idField = "signature")
  }

  def issue(from: Account, name: String, description: String, quantity: Long, decimals: Byte, reissuable: Boolean, fee: Long = 100000L, assertSuccess: Boolean = true): Option[String] = {
    val json = Json.toJson(IssueRequest(from.address, name, description, quantity, decimals, reissuable, fee)).toString()
    makeTxRequest(POST.requestRaw(us = "/assets/issue", body = json), assertSuccess)
  }

  def reissue(from: Account, assetId: String, quantity: Long, reissuable: Boolean, fee: Long = 100000L, assertSuccess: Boolean = true): Option[String] = {
    val json = Json.toJson(ReissueRequest(from.address, assetId, quantity, reissuable, fee)).toString()
    makeTxRequest(POST.requestRaw(us = "/assets/reissue", body = json), assertSuccess)
  }

  def burn(from: Account, assetId: String, quantity: Long, fee: Long = 100000L, assertSuccess: Boolean = true): Option[String] = {
    val json = Json.toJson(BurnRequest(from.address, assetId, quantity, fee)).toString()
    makeTxRequest(POST.requestRaw(us = "/assets/burn", body = json), assertSuccess)
  }

  def lease(from: Account, to: Account, amount: Long, fee: Long = 100000L, assetId: Option[String] = None, assertSuccess: Boolean = true): Option[String] = {
    val json = Json.toJson(LeaseRequest(from.address, amount, fee, to.address)).toString()
    makeTxRequest(POST.requestRaw(us = "/leasing/lease", body = json), assertSuccess)
  }

  def cancelLease(from: Account, leaseTxId: String, fee: Long = 100000L, assertSuccess: Boolean = true): Unit = {
    val json = Json.toJson(LeaseCancelRequest(from.address, leaseTxId, fee)).toString()
    makeTxRequest(POST.requestRaw(us = "/leasing/cancel", body = json), assertSuccess)
  }

  def makeTxRequest(request: => Response, assertSuccess: Boolean = true, idField: String = "id"): Option[String] = {
    val response = request
    if (assertSuccess) {
      require(response.getStatusCode == 200)
      waitForNextBlock(application)
      Some((Json.parse(response.getResponseBody) \ idField).asOpt[String].get) // id should exists
    } else {
      require(response.getStatusCode != 200)
      None
    }
  }

  def waitForBalance(balance: Long, acc: Account, asset: Option[String] = None)(implicit storedState: StoredState): Unit = {
    val assetId = asset.flatMap(Base58.decode(_).toOption)
    eventually(timeout(5.seconds), interval(500.millis)) {
      Thread.sleep(100)
      require(storedState.assetBalance(AssetAcc(acc, assetId)) == balance)
    }
  }

  def waitForEffectiveBalance(balance: Long, acc: Account)(implicit storedState: StoredState): Unit = {
     eventually(timeout(5.seconds), interval(500.millis)) {
      Thread.sleep(100)
       require(storedState.effectiveBalance(acc) == balance)
    }
  }

  def forgeSignature(signature: Array[Byte]): Array[Byte] = {
    val modifier: BigInt = BigInt("7237005577332262213973186563042994240857116359379907606001950938285454250989")
    signature.take(32) ++ (BigInt(signature.takeRight(32).reverse) + modifier).toByteArray.reverse
  }

  def peerUrl(a: Application = application): String =
    "http://" + a.settings.restAPISettings.bindAddress + ":" + a.settings.restAPISettings.port

  def matcherUrl(a: Application = application): String =
    "http://" + a.settings.matcherSettings.bindAddress + ":" + a.settings.matcherSettings.port + "/matcher"

  def startGeneration(nodes: Seq[Application]): Unit = {
    nodes.foreach(_.blockGenerator ! StartGeneration)
  }

  def stopGeneration(nodes: Seq[Application]): Unit = {
    nodes.foreach(_.blockGenerator ! StopGeneration)
    untilTimeout(5.seconds) {
      Thread.sleep(100)
      nodes.foreach { p =>
        require(Await.result(p.blockGenerator ? GetBlockGenerationStatus, timeout.duration) == Idle.name)
      }
    }
  }

  object Request {
    type RequestMethodSetter = Req => Req

    val POST: RequestMethodSetter = _.POST
    val GET: RequestMethodSetter = _.GET
    val OPTIONS: RequestMethodSetter = _.OPTIONS
    val DELETE: RequestMethodSetter = _.DELETE
  }

  sealed trait Request {
    def t: Request.RequestMethodSetter

    def incorrectApiKeyTest(path: String): Unit = {
      Seq(Map[String, String](), Map("api_key" -> "wrong key")) foreach { h =>
        val resp = requestJson(path, headers = h).toString()
        assert(resp == ApiKeyNotValid.json.toString(), s"$resp == ${ApiKeyNotValid.json.toString()} is false")
      }
    }

    def requestJson(us: String,
                    params: Map[String, String] = Map.empty,
                    body: String = "",
                    headers: Map[String, String] = Map("api_key" -> "test", "Content-type" -> "application/json"),
                    peer: String = peerUrl(application)): JsValue = _requestJson(us, params, body, headers, peer, t)

    def requestRaw(us: String,
                   params: Map[String, String] = Map.empty,
                   body: String = "",
                   headers: Map[String, String] = Map("api_key" -> "test", "Content-type" -> "application/json"),
                   peer: String = peerUrl(application)): Response = _requestRaw(us, params, body, headers, peer, t)

    def requestObject[T](us: String,
                         params: Map[String, String] = Map.empty,
                         body: String = "",
                         headers: Map[String, String] = Map.empty,
                         peer: String = peerUrl(application))(implicit format: Format[T]): T = _requestObject(us, params, body, headers, peer, t)

    protected def _requestJson(us: String,
                               params: Map[String, String] = Map.empty,
                               body: String = "",
                               headers: Map[String, String] = Map("api_key" -> "test", "Content-type" -> "application/json"),
                               peer: String = peerUrl(application),
                               method: Req => Req): JsValue = {
      val raw = _requestRaw(us, params, body, headers, peer, method)
      Json.parse(raw.getResponseBody)
    }

    protected def _requestRaw(us: String,
                              params: Map[String, String] = Map.empty,
                              body: String = "",
                              headers: Map[String, String] = Map("api_key" -> "test", "Content-type" -> "application/json"),
                              peer: String = peerUrl(application),
                              method: Req => Req): Response = {
      val request = method match {
        case Request.POST =>
          method(url(peer + us) <:< headers <<? params << body)
        case _ =>
          method(url(peer + us) <:< headers <<? params)
      }
      Await.result(Http(request), timeout)
    }

    private val timeout = 5.seconds

    protected def _requestObject[T](us: String,
                                    params: Map[String, String] = Map.empty,
                                    body: String = "",
                                    headers: Map[String, String] = Map("api_key" -> "test"),
                                    peer: String = peerUrl(application),
                                    method: Req => Req)(implicit format: Format[T]): T = {
      format.reads(_requestJson(us, params, body, headers, peer, method)).getOrElse(throw new RuntimeException)
    }
  }

  case object GET extends Request {
    override val t: Request.RequestMethodSetter = Request.GET
  }

  case object POST extends Request {
    override val t: Request.RequestMethodSetter = Request.POST
  }

  case object OPTIONS extends Request {
    override val t: Request.RequestMethodSetter = Request.OPTIONS
  }

  case object DELETE extends Request {
    override val t: Request.RequestMethodSetter = Request.DELETE
  }

}
