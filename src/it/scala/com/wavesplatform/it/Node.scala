package com.wavesplatform.it

import java.io.IOException

import com.typesafe.config.Config
import com.wavesplatform.it.util._
import com.wavesplatform.settings.WavesSettings
import io.netty.util.Timer
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.slf4j.LoggerFactory
import play.api.libs.json.Json._
import play.api.libs.json._
import scorex.api.http.alias.CreateAliasRequest
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.transaction.TransactionParser.TransactionType
import scorex.utils.{LoggerFacade, ScorexLogging}

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


class Node(config: Config, nodeInfo: NodeInfo, client: AsyncHttpClient, timer: Timer) {

  import Node._

  val privateKey = config.getString("private-key")
  val publicKey = config.getString("public-key")
  val address = config.getString("address")
  val settings = WavesSettings.fromConfig(config)

  private val log = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getName}.${settings.networkSettings.nodeName}"))

  def fee(txValue: TransactionType.Value, asset: String = "WAVES"): Long =
    settings.feesSettings.fees(txValue.id).find(_.asset == asset).get.fee

  private def retrying(r: Request, interval: FiniteDuration = 1.second): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.trace(s"$r")
      client.executeRequest(r).toCompletableFuture.toScala
        .recoverWith {
          case t: Throwable =>
            log.debug("Retrying request", t)
            executeRequest
        }
    }

    executeRequest
  }

  def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"http://localhost:${nodeInfo.hostRestApiPort}$path")).build())

  def post(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(
      _post(s"http://localhost:${nodeInfo.hostRestApiPort}$path").setHeader("api_key", "integration-test-rest-api")
    ).build())

  def post[A: Writes](path: String, body: A): Future[Response] =
    post(path, (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

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

  def assetBalance(address: String, assetId: String): Future[Long] = get(s"/assets/balance/$address/$assetId").as[JsValue].map(v => (v \ "balance").as[Long])

  def waitForTransaction(txId: String): Future[Transaction] = waitFor[Option[Transaction]](transactionInfo(txId), t => t.isDefined, 1.second).map(_.get)

  def waitForHeight(expectedHeight: Long): Future[Long] = waitFor[Long](height, h => h >= expectedHeight, 1.second)

  def transactionInfo(txId: String): Future[Option[Transaction]] = get(s"/transactions/info/$txId").as[Transaction].transform {
    case Success(info) => Success(Some(info))
    case Failure(_: IOException) => Success(None)
    case Failure(ex) => Failure(ex)
  }

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

  def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Future[Transaction] =
    post("/assets/reissue", ReissueRequest(sourceAddress, assetId, quantity, reissuable, fee)).as[Transaction]

  def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long): Future[Transaction] =
    post("/assets/burn", BurnRequest(sourceAddress, assetId, quantity, fee)).as[Transaction]

  def createAlias(targetAddress: String, alias: String, fee: Long): Future[Transaction] =
    post("/alias/create", CreateAliasRequest(targetAddress, alias, fee)).as[Transaction]

  def waitFor[A](f: => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] =
    timer.retryUntil(f, cond, retryInterval)

  def createAddress: Future[String] =
    post("/addresses").as[JsValue].map(v => (v \ "address").as[String])
}

object Node extends ScorexLogging {
  case class Status(blockGeneratorStatus: String, historySynchronizationStatus: String)
  implicit val statusFormat: Format[Status] = Json.format

  case class Peer(address: String, declaredAddress: String, peerName: String)
  implicit val peerFormat: Format[Peer] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)
  implicit val balanceFormat: Format[Balance] = Json.format

  case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long)
  implicit val transactionFormat: Format[Transaction] = Json.format

  case class Block(signature: String, height: Long, timestamp: Long, generator: String, transactions: Seq[Transaction])
  implicit val blockFormat: Format[Block] = Json.format

  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext): Future[A] =
      f.transform {
        case Success(r) if r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200 =>
          Try(parse(r.getResponseBody).as[A])
        case Success(r) =>
          log.debug(s"Error parsing response ${r.getResponseBody}")
          Failure(new IOException(s"Unexpected status code: ${r.getStatusCode}"))
        case Failure(t) => Failure[A](t)
      }(ec)
  }
}
