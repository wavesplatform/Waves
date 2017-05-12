package com.wavesplatform.it

import java.io.IOException

import com.typesafe.config.Config
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.settings.WavesSettings
import io.netty.util.Timer
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient.util.HttpConstants
import org.asynchttpclient.{Response, _}
import org.slf4j.LoggerFactory
import play.api.libs.json.Json._
import play.api.libs.json._
import scorex.api.http.alias.CreateAliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.assets.exchange.Order
import scorex.utils.{LoggerFacade, ScorexLogging}

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.util.{Failure, Success, Try}


class Node(config: Config, nodeInfo: NodeInfo, client: AsyncHttpClient, timer: Timer) {

  import Node._

  val privateKey: String = config.getString("private-key")
  val publicKey: String = config.getString("public-key")
  val address: String = config.getString("address")
  val accountSeed: String = config.getString("account-seed")
  val settings: WavesSettings = WavesSettings.fromConfig(config)

  private val blockDelay = settings.blockchainSettings.genesisSettings.averageBlockDelay
  private implicit val log = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getName}.${settings.networkSettings.nodeName}"))

  def fee(txValue: TransactionType.Value, asset: String = "WAVES"): Long =
    settings.feesSettings.fees(txValue.id).find(_.asset == asset).get.fee

  private def retrying(r: Request, interval: FiniteDuration = 1.second): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.trace(s"Executing request '$r'")
      client.executeRequest(r).toCompletableFuture.toScala
        .recoverWith {
          case e@(_: IOException | _: TimeoutException) =>
            log.debug(s"Failed to execute request '$r' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }

  def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"http://localhost:${nodeInfo.hostRestApiPort}$path")).build())

  def matcherGet(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"http://localhost:${nodeInfo.hostMatcherApiPort}$path")).build())

  def post(url: String, port: Int, path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(
      _post(s"$url:$port$path").setHeader("api_key", "integration-test-rest-api")
    ).build())

  def post[A: Writes](path: String, body: A): Future[Response] =
    post("http://localhost", nodeInfo.hostRestApiPort, path,
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

  def matcherPost[A: Writes](path: String, body: A): Future[Response] =
    post("http://localhost", nodeInfo.hostMatcherApiPort, path,
      (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))


  def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
    (Json.parse(r.getResponseBody) \ "peers").as[Seq[Peer]]
  }

  def connectedPeersCount: Future[Int] = get("/peers/connected").map { r =>
    (Json.parse(r.getResponseBody) \ "peers").as[Seq[Peer]]
  }.map(_.length)

  def waitForPeers(targetPeersCount: Int): Future[Int] = waitFor[Int](connectedPeersCount, _ >= targetPeersCount, 1.second)

  def height: Future[Long] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Long])

  def blockAt(height: Long) = get(s"/blocks/at/$height").as[Block]

  def lastBlock: Future[Block] = get("/blocks/last").as[Block]

  def blockSeq(from: Long, to: Long) = get(s"/blocks/seq/$from/$to").as[Seq[Block]]

  def status: Future[Status] = get("/node/status").as[Status]

  def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]

  def waitForTransaction(txId: String): Future[Transaction] = {
    log.debug(s"waiting for tx=$txId")
      waitFor[Option[Transaction]] (transactionInfo (txId).transform {
      case Success (tx) => Success (Some (tx) )
      case Failure (UnexpectedStatusCodeException (r) ) if r.getStatusCode == 404 => Success (None)
      case Failure (ex) => Failure (ex)
      }, tOpt => tOpt.exists(_.id == txId), 1.second).map(_.get)
      }

  def waitForHeight(expectedHeight: Long): Future[Long] = waitFor[Long](height, h => h >= expectedHeight, 1.second)

  def transactionInfo(txId: String): Future[Transaction] = get(s"/transactions/info/$txId").as[Transaction]

  def effectiveBalance(address: String): Future[Balance] = get(s"/addresses/effectiveBalance/$address").as[Balance]

  def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long, assetId: Option[String] = None): Future[Transaction] =
    post("/assets/transfer", TransferRequest(assetId, None, amount, fee, sourceAddress, None, recipient)).as[Transaction]

  def payment(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[String] =
    post("/waves/payment", PaymentRequest(amount, fee, sourceAddress, recipient)).as[JsValue].map(v => (v \ "signature").as[String])

  def lease(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
    post("/leasing/lease", LeaseRequest(sourceAddress, amount, fee, recipient)).as[Transaction]

  def cancelLease(sourceAddress: String, leaseId: String, fee: Long): Future[Transaction] =
    post("/leasing/cancel", LeaseCancelRequest(sourceAddress, leaseId, fee)).as[Transaction]

  def issue(sourceAddress: String, name: String, description: String, quantity: Long, decimals: Byte, reissuable: Boolean, fee: Long): Future[Transaction] =
    post("/assets/issue", IssueRequest(sourceAddress, name, description, quantity, decimals, reissuable, fee)).as[Transaction]

  def makeAssetNameUnique(sourceAddress: String, assetId: String, fee: Long, networkByte: Byte): Future[Transaction] =
    post("/assets/make-asset-name-unique", MakeAssetNameUniqueRequest(sourceAddress, assetId, fee, networkByte)).as[Transaction]

  def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Future[Transaction] =
    post("/assets/reissue", ReissueRequest(sourceAddress, assetId, quantity, reissuable, fee)).as[Transaction]

  def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long): Future[Transaction] =
    post("/assets/burn", BurnRequest(sourceAddress, assetId, quantity, fee)).as[Transaction]

  def assetBalance(address: String, asset: String): Future[AssetBalance] =
    get(s"/assets/balance/$address/$asset").as[AssetBalance]

  def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
    post("/assets/transfer", TransferRequest(None, None, amount, fee, sourceAddress, None, recipient)).as[Transaction]

  def createAlias(targetAddress: String, alias: String, fee: Long): Future[Transaction] =
    post("/alias/create", CreateAliasRequest(targetAddress, alias, fee)).as[Transaction]

  def waitFor[A](f: => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] =
    timer.retryUntil(f, cond, retryInterval)

  def createAddress: Future[String] =
    post("http://localhost", nodeInfo.hostRestApiPort, "/addresses").as[JsValue].map(v => (v \ "address").as[String])

  def waitForNextBlock: Future[Block] = for {
    currentBlock <- lastBlock
    actualBlock <- findBlock(_.height > currentBlock.height, currentBlock.height)
  } yield actualBlock

  def findBlock(cond: Block => Boolean, from: Long = 1, to: Long = Long.MaxValue): Future[Block] = {
    def load(_from: Long, _to: Long): Future[Block] = blockSeq(_from, _to).flatMap { blocks =>
      blocks.find(cond).fold[Future[Node.Block]] {
        val maybeLastBlock = blocks.lastOption
        if (maybeLastBlock.exists(_.height >= to)) {
          Future.failed(new NoSuchElementException)
        } else {
          val newFrom = maybeLastBlock.fold(_from)(b => (b.height + 19L).min(to))
          val newTo = newFrom + 19
          log.debug(s"Loaded ${blocks.length} blocks, no match found. Next range: [$newFrom, ${newFrom + 19}]")
          timer.schedule(load(newFrom, newTo), blockDelay)
        }
      }(Future.successful)
    }

    load(from, (from + 19).min(to))
  }

  def getGeneratedBlocks(address: String, from: Long, to: Long): Future[Seq[Block]] =
    get(s"/blocks/address/$address/$from/$to").as[Seq[Block]]

  def issueAsset(address: String, name: String, description: String, quantity: Long, decimals: Byte, fee: Long,
                 reissuable: Boolean): Future[Transaction] =
    post("/assets/issue", IssueRequest(address, name, description, quantity, decimals, reissuable, fee)).as[Transaction]

  def placeOrder(order: Order): Future[MatcherResponse] =
    matcherPost("/matcher/orderbook", order.json).as[MatcherResponse]

  def expectIncorrectOrderPlacement(order: Order, expectedStatusCode: Int, expectedStatus: String): Future[Boolean] =
    matcherPost("/matcher/orderbook", order.json) transform {
      case Success(r) if r.getStatusCode == expectedStatusCode =>
        Try(parse(r.getResponseBody).as[MatcherStatusResponse]) match {
          case Success(mr) if mr.status == expectedStatus => Success(true)
          case Failure(f) => Failure(new RuntimeException(s"Failed to parse response: $f"))
        }
      case Success(r) => Failure(new RuntimeException(s"Unexpected matcher response: (${r.getStatusCode}) ${r.getResponseBody}"))
      case _ => Failure(new RuntimeException(s"Unexpected failure from matcher"))
    }

  def getOrderStatus(asset: String, orderId: String): Future[MatcherStatusResponse] =
    matcherGet(s"/matcher/orderbook/$asset/WAVES/$orderId").as[MatcherStatusResponse]

  def getOrderBook(asset: String): Future[OrderBookResponse] =
    matcherGet(s"/matcher/orderbook/$asset/WAVES").as[OrderBookResponse]

  def cancelOrder(amountAsset: String, priceAsset: String, request: CancelOrderRequest): Future[MatcherStatusResponse] =
    matcherPost(s"/matcher/orderbook/$amountAsset/$priceAsset/cancel", request.json).as[MatcherStatusResponse]
}

object Node {

  case class UnexpectedStatusCodeException(r: Response) extends IOException(s"Unexpected status code: ${r.getStatusCode}")

  case class Status(blockGeneratorStatus: Option[String], historySynchronizationStatus: Option[String])

  implicit val statusFormat: Format[Status] = Json.format

  case class Peer(address: String, declaredAddress: String, peerName: String)

  implicit val peerFormat: Format[Peer] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)

  implicit val balanceFormat: Format[Balance] = Json.format

  case class AssetBalance(address: String, assetId: String, balance: Long)

  implicit val assetBalanceFormat: Format[AssetBalance] = Json.format

  case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long)

  implicit val transactionFormat: Format[Transaction] = Json.format

  case class Block(signature: String, height: Long, timestamp: Long, generator: String, transactions: Seq[Transaction],
                   fee: Long)

  implicit val blockFormat: Format[Block] = Json.format

  case class MatcherMessage(id: String)

  implicit val matcherMessageFormat: Format[MatcherMessage] = Json.format

  case class MatcherResponse(status: String, message: MatcherMessage)

  implicit val matcherResponseFormat: Format[MatcherResponse] = Json.format

  case class MatcherStatusResponse(status: String)

  implicit val matcherStatusResponseFormat: Format[MatcherStatusResponse] = Json.format

  case class PairResponse(amountAsset: String, priceAsset: String)

  implicit val pairResponseFormat: Format[PairResponse] = Json.format

  case class LevelResponse(price: Long, amount: Long)

  implicit val levelResponseFormat: Format[LevelResponse] = Json.format

  case class OrderBookResponse(timestamp: Long, pair: PairResponse, bids: List[LevelResponse], asks: List[LevelResponse])

  implicit val orderBookResponseFormat: Format[OrderBookResponse] = Json.format

  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext, log: LoggerFacade): Future[A] =
      f.transform {
        case Success(r) if r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200 =>
          log.debug(s"Response: ${r.getResponseBody}")
          Try(parse(r.getResponseBody).as[A])
        case Success(r) =>
          log.debug(s"Error parsing response ${r.getResponseBody}")
          Failure(UnexpectedStatusCodeException(r))
        case Failure(t) => Failure[A](t)
      }(ec)
  }

}
