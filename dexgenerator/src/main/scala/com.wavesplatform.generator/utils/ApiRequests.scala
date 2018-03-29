package com.wavesplatform.generator.utils

import java.io.IOException
import java.util.concurrent.TimeoutException

import com.google.common.primitives.Longs
import com.wavesplatform.crypto
import com.wavesplatform.it.api.{
  AssetBalance,
  Balance,
  MatcherResponse,
  MatcherStatusResponse,
  OrderbookHistory,
  ResponseFutureExt,
  Transaction,
  UnexpectedStatusCodeException
}
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state2.ByteStr
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import play.api.libs.json.Json.{stringify, toJson}
import play.api.libs.json._
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.api.http.assets.{MassTransferRequest, SignedIssueRequest, SignedMassTransferRequest}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.MassTransferTransaction.{ParsedTransfer, Transfer}
import scorex.transaction.assets.{IssueTransaction, MassTransferTransaction}
import scorex.transaction.assets.exchange.Order
import scorex.utils.ScorexLogging

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class ApiRequests(client: AsyncHttpClient) extends ScorexLogging {

  def retrying(r: Request, interval: FiniteDuration = 1.second, statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.info(s"Executing request '$r'")
      client
        .executeRequest(
          r,
          new AsyncCompletionHandler[Response] {
            override def onCompleted(response: Response): Response = {
              if (response.getStatusCode == statusCode) {
                log.info(s"Request: ${r.getUrl}\nResponse: ${response.getResponseBody}")
                response
              } else {
                log.info(s"Request: ${r.getUrl}\nUnexpected status code(${response.getStatusCode}): ${response.getResponseBody}")
                throw UnexpectedStatusCodeException(r.getUrl, response.getStatusCode, response.getResponseBody)
              }
            }
          }
        )
        .toCompletableFuture
        .toScala
        .recoverWith {
          case e @ (_: IOException | _: TimeoutException) =>
            log.info(s"Failed to execute request '$r' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }

  def createSignedIssueRequest(tx: IssueTransaction): SignedIssueRequest = {
    import tx._
    SignedIssueRequest(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }

  def createSignedMassTransferRequest(tx: MassTransferTransaction): SignedMassTransferRequest = {
    import tx._
    SignedMassTransferRequest(
      MassTransferTransaction.Version,
      Base58.encode(tx.sender.publicKey),
      assetId.map(_.base58),
      transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) },
      fee,
      timestamp,
      attachment.headOption.map(_ => Base58.encode(attachment)),
      proofs.base58().toList
    )
  }

  def to(endpoint: String) = new Node(endpoint)

  class Node(endpoint: String) {

    def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(_get(s"$endpoint$path")).build())

    def post(url: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(_post(url)).build())

    def post(path: String, body: String): Future[Response] =
      post(s"$endpoint$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

    def postJson[A: Writes](path: String, body: A): Future[Response] =
      post(path, stringify(toJson(body)))

    def matcherPost[A: Writes](path: String, body: A): Future[Response] =
      post(s"$endpoint$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

    def placeOrder(order: Order): Future[MatcherResponse] =
      matcherPost("/matcher/orderbook", order.json()).as[MatcherResponse]

    def height(endpoint: String): Future[Int] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Int])

    def transactionInfo(txId: String): Future[Transaction] = get(s"/transactions/info/$txId").as[Transaction]

    def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]

    def assetBalance(address: String, asset: String): Future[AssetBalance] =
      get(s"/assets/balance/$address/$asset").as[AssetBalance]

    def signedIssue(issue: SignedIssueRequest): Future[Transaction] =
      postJson("/assets/broadcast/issue", issue).as[Transaction]

    //    def signedMassTransfer(massTx: SignedMassTransferRequest): Future[Transaction] =
    //      postJson("/assets/broadcast/issue", massTx).as[Transaction]

    def orderbookByPublicKey(publicKey: String,
                             ts: Long,
                             signature: ByteStr,
                             f: RequestBuilder => RequestBuilder = identity): Future[Seq[OrderbookHistory]] =
      retrying {
        _get(s"$endpoint/matcher/orderbook/$publicKey")
          .setHeader("Timestamp", ts)
          .setHeader("Signature", signature)
          .build()
      }.as[Seq[OrderbookHistory]]

    //    def getOrderbookByPublicKey(publicKey: String, timestamp: Long, signature: ByteStr): Future[Seq[OrderbookHistory]] =
    //      matcherGetWithSignature(s"/matcher/orderbook/$publicKey", timestamp, signature).as[Seq[OrderbookHistory]]

    def cancelOrder(amountAsset: String, priceAsset: String, request: CancelOrderRequest): Future[MatcherStatusResponse] =
      matcherPost(s"/matcher/orderbook/$amountAsset/$priceAsset/cancel", request.json).as[MatcherStatusResponse]

    def broadcastRequest[A: Writes](req: A): Future[Transaction] = postJson("/transactions/broadcast", req).as[Transaction]

    def orderHistory(pk: PrivateKeyAccount): Seq[OrderbookHistory] = {
      val ts         = System.currentTimeMillis()
      val privateKey = pk
      val signature  = ByteStr(crypto.sign(pk, pk.publicKey ++ Longs.toByteArray(ts)))
      Await.result(orderbookByPublicKey(Base58.encode(pk.publicKey), ts, signature), 1.minute)
    }

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): Future[Transaction] =
      waitFor[Option[Transaction]](s"transaction $txId")(
        _.transactionInfo(txId).transform {
          case Success(tx)                                       => Success(Some(tx))
          case Failure(UnexpectedStatusCodeException(_, 404, _)) => Success(None)
          case Failure(ex)                                       => Failure(ex)
        },
        tOpt => tOpt.exists(_.id == txId),
        retryInterval
      ).map(_.get)

    private val RequestAwaitTime = 15.seconds

    def waitFor[A](desc: String)(f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] = {
      log.debug(s"Awaiting condition '$desc'")
      timer
        .retryUntil(f(this), cond, retryInterval)
        .map(a => {
          log.debug(s"Condition '$desc' met")
          a
        })
    }
  }

}
