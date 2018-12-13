package com.wavesplatform.dexgen.utils

import java.io.IOException
import java.util.concurrent.TimeoutException

import com.google.common.primitives.Longs
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.api.http.assets.{SignedIssueV1Request, SignedMassTransferRequest, SignedTransferV1Request}
import com.wavesplatform.crypto
import com.wavesplatform.it.api.{
  AssetBalance,
  Balance,
  MatcherResponse,
  MatcherStatusResponse,
  OrderBookResponse,
  OrderbookHistory,
  ResponseFutureExt,
  Transaction,
  UnexpectedStatusCodeException
}
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{ParsedTransfer, Transfer}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransactionV1}
import com.wavesplatform.utils.ScorexLogging
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import play.api.libs.json.Json.{stringify, toJson}
import play.api.libs.json._
import scorex.crypto.encode.Base58

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class ApiRequests(client: AsyncHttpClient) extends ScorexLogging {

  def retrying(r: Request, interval: FiniteDuration = 1.second, statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200)(
      implicit tag: String): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.info(s"[$tag] Executing request '$r'")
      client
        .executeRequest(
          r,
          new AsyncCompletionHandler[Response] {
            override def onCompleted(response: Response): Response = {
              if (response.getStatusCode == statusCode) {
                log.info(s"[$tag] Request: ${r.getUrl}\nResponse: ${response.getResponseBody}")
                response
              } else {
                log.info(s"[$tag] Request: ${r.getUrl}\nUnexpected status code(${response.getStatusCode}): ${response.getResponseBody}")
                throw UnexpectedStatusCodeException(r.getUrl, response.getStatusCode, response.getResponseBody)
              }
            }
          }
        )
        .toCompletableFuture
        .toScala
        .recoverWith {
          case e @ (_: IOException | _: TimeoutException) =>
            log.info(s"[$tag] Failed to execute request '$r' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
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
    SignedMassTransferRequest(
      MassTransferTransaction.version,
      Base58.encode(tx.sender.publicKey),
      tx.assetId.map(_.base58),
      tx.transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) },
      tx.fee,
      tx.timestamp,
      tx.attachment.headOption.map(_ => Base58.encode(tx.attachment)),
      tx.proofs.base58().toList
    )
  }

  def createSignedTransferRequest(tx: TransferTransactionV1): SignedTransferV1Request = {

    SignedTransferV1Request(
      Base58.encode(tx.sender.publicKey),
      tx.assetId.map(_.base58),
      tx.recipient.stringRepr,
      tx.amount,
      tx.fee,
      tx.feeAssetId.map(_.base58),
      tx.timestamp,
      tx.attachment.headOption.map(_ => Base58.encode(tx.attachment)),
      tx.signature.base58
    )
  }

  def to(endpoint: String) = new Node(endpoint)

  class Node(endpoint: String) {

    def get(path: String, f: RequestBuilder => RequestBuilder = identity)(implicit tag: String): Future[Response] =
      retrying(f(_get(s"$endpoint$path")).build())

    def post(url: String, f: RequestBuilder => RequestBuilder = identity)(implicit tag: String): Future[Response] =
      retrying(f(_post(url)).build())

    def post(path: String, body: String)(implicit tag: String): Future[Response] =
      post(s"$endpoint$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

    def postJson[A: Writes](path: String, body: A)(implicit tag: String): Future[Response] =
      post(path, stringify(toJson(body)))

    def matcherPost[A: Writes](path: String, body: A)(implicit tag: String): Future[Response] =
      post(s"$endpoint$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

    def matcherGet(path: String, f: RequestBuilder => RequestBuilder = identity, statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200)(
        implicit tag: String): Future[Response] =
      retrying(f(_get(s"$endpoint$path")).build(), statusCode = statusCode)

    def placeOrder(order: Order)(implicit tag: String): Future[MatcherResponse] =
      matcherPost("/matcher/orderbook", order.json()).as[MatcherResponse]

    def height(endpoint: String)(implicit tag: String): Future[Int] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Int])

    def transactionInfo(txId: String)(implicit tag: String): Future[Transaction] = get(s"/transactions/info/$txId").as[Transaction]

    def balance(address: String)(implicit tag: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]

    def assetBalance(address: String, asset: String)(implicit tag: String): Future[AssetBalance] =
      get(s"/assets/balance/$address/$asset").as[AssetBalance]

    def balance(address: String, asset: Option[AssetId])(implicit tag: String): Future[Long] = asset match {
      case None => to(endpoint).balance(address).map(_.balance)
      case _    => to(endpoint).assetBalance(address, asset.map(_.base58).get).map(_.balance)
    }

    def signedIssue(issue: SignedIssueV1Request)(implicit tag: String): Future[Transaction] =
      postJson("/assets/broadcast/issue", issue).as[Transaction]

    def orderbookByPublicKey(publicKey: String, ts: Long, signature: ByteStr, f: RequestBuilder => RequestBuilder = identity)(
        implicit tag: String): Future[Seq[OrderbookHistory]] =
      retrying {
        _get(s"$endpoint/matcher/orderbook/$publicKey")
          .setHeader("Timestamp", ts)
          .setHeader("Signature", signature)
          .build()
      }.as[Seq[OrderbookHistory]]

    def parseAssetPair(assetPair: AssetPair): (String, String) = {
      val amountAsset = AssetPair.assetIdStr(assetPair.amountAsset)
      val priceAsset  = AssetPair.assetIdStr(assetPair.priceAsset)
      (amountAsset, priceAsset)
    }

    def orderBook(assetPair: AssetPair)(implicit tag: String): Future[OrderBookResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset").as[OrderBookResponse]
    }

    def orderStatus(orderId: String, assetPair: AssetPair)(implicit tag: String): Future[MatcherStatusResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset/$orderId")
        .as[MatcherStatusResponse]
    }

    def cancelOrder(amountAsset: String, priceAsset: String, request: CancelOrderRequest)(implicit tag: String): Future[MatcherStatusResponse] =
      matcherPost(s"/matcher/orderbook/$amountAsset/$priceAsset/cancel", request).as[MatcherStatusResponse]

    def broadcastRequest[A: Writes](req: A)(implicit tag: String): Future[Transaction] = postJson("/transactions/broadcast", req).as[Transaction]

    def orderHistory(pk: PrivateKeyAccount)(implicit tag: String): Future[Seq[OrderbookHistory]] = {
      val ts        = System.currentTimeMillis()
      val signature = ByteStr(crypto.sign(pk, pk.publicKey ++ Longs.toByteArray(ts)))
      orderbookByPublicKey(Base58.encode(pk.publicKey), ts, signature)
    }

    def utx(implicit tag: String): Future[Seq[Transaction]] = get(s"/transactions/unconfirmed").as[Seq[Transaction]]

    def unconfirmedTxInfo(txId: String)(implicit tag: String): Future[Transaction] = get(s"/transactions/unconfirmed/info/$txId").as[Transaction]

    def findTransactionInfo(txId: String)(implicit tag: String): Future[Option[Transaction]] = transactionInfo(txId).transform {
      case Success(tx)                                          => Success(Some(tx))
      case Failure(UnexpectedStatusCodeException(_, _, 404, _)) => Success(None)
      case Failure(ex)                                          => Failure(ex)
    }

    def ensureTxDoesntExist(txId: String)(implicit tag: String): Future[Unit] =
      utx
        .zip(findTransactionInfo(txId))
        .flatMap({
          case (utx, _) if utx.map(_.id).contains(txId) =>
            Future.failed(new IllegalStateException(s"[$tag] Tx $txId is in UTX"))
          case (_, txOpt) if txOpt.isDefined =>
            Future.failed(new IllegalStateException(s"[$tag] Tx $txId is in blockchain"))
          case _ =>
            Future.successful(())
        })

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second)(implicit tag: String): Future[Transaction] =
      waitFor[Option[Transaction]](s"transaction $txId")(
        _.transactionInfo(txId)
          .map(x => Option(x))
          .recoverWith {
            case e: UnexpectedStatusCodeException if e.statusCode == 404 => Future.successful(None)
          },
        _.exists(_.id == txId),
        retryInterval
      ).map(_.get)

    def waitFor[A](desc: String)(f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration)(implicit tag: String): Future[A] = {
      log.debug(s"[$tag] Awaiting condition '$desc'")
      timer
        .retryUntil(f(this), cond, retryInterval)
        .map(a => {
          log.debug(s"[$tag] Condition '$desc' met")
          a
        })
    }
  }

}
