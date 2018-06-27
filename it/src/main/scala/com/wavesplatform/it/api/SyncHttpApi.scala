package com.wavesplatform.it.api

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.it.Node
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state.{ByteStr, DataEntry}
import org.asynchttpclient.util.HttpConstants
import org.asynchttpclient.{RequestBuilder, Response}
import org.scalactic.source.Position
import org.scalatest.{Assertion, Assertions, Matchers}
import play.api.libs.json.Json.parse
import play.api.libs.json.{Format, JsObject, Json, Writes}
import scorex.api.http.AddressApiRoute
import scorex.api.http.assets.SignedIssueV1Request
import scorex.transaction.assets.exchange.Order
import scorex.transaction.transfer.MassTransferTransaction.Transfer

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Try}

object SyncHttpApi extends Assertions {
  case class ErrorMessage(error: Int, message: String)

  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

  def assertBadRequest[R](f: => R): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, _)) => Assertions.assert(statusCode == StatusCodes.BadRequest.intValue)
    case Failure(e)                                               => Assertions.fail(e)
    case _                                                        => Assertions.fail("Expecting bad request")
  }

  def assertBadRequestAndResponse[R](f: => R, errorRegex: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      Assertions.assert(
        statusCode == StatusCodes.BadRequest.intValue &&
          responseBody.replace("\n", "").matches(s".*$errorRegex.*"))
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail("Expecting bad request")
  }

  def assertBadRequestAndMessage[R](f: => R, errorMessage: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      Assertions.assert(statusCode == StatusCodes.BadRequest.intValue && parse(responseBody).as[ErrorMessage].message.contains(errorMessage))
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail(s"Expecting bad request")
  }

  implicit class NodeExtSync(n: Node) extends Assertions with Matchers {

    import com.wavesplatform.it.api.AsyncHttpApi.{NodeAsyncHttpApi => async}

    private val RequestAwaitTime      = 15.seconds
    private val OrderRequestAwaitTime = 1.minutes

    def get(path: String): Response =
      Await.result(async(n).get(path), RequestAwaitTime)

    def utx = Await.result(async(n).utx, RequestAwaitTime)

    def utxSize = Await.result(async(n).utxSize, RequestAwaitTime)

    def seed(address: String): String =
      Await.result(async(n).seed(address), RequestAwaitTime)

    def postJson[A: Writes](path: String, body: A): Response =
      Await.result(async(n).postJson(path, body), RequestAwaitTime)

    def postJsonWithApiKey[A: Writes](path: String, body: A): Response =
      Await.result(async(n).postJsonWithApiKey(path, body), RequestAwaitTime)

    def accountBalances(acc: String): (Long, Long) =
      Await.result(async(n).accountBalances(acc), RequestAwaitTime)

    def assertBalances(acc: String, balance: Long)(implicit pos: Position): Unit =
      Await.result(async(n).assertBalances(acc, balance, effectiveBalance = balance), RequestAwaitTime)

    def assertBalances(acc: String, balance: Long, effectiveBalance: Long)(implicit pos: Position): Unit =
      Await.result(async(n).assertBalances(acc, balance, effectiveBalance), RequestAwaitTime)

    def assertAssetBalance(acc: String, assetIdString: String, balance: Long)(implicit pos: Position): Unit =
      Await.result(async(n).assertAssetBalance(acc, assetIdString, balance), RequestAwaitTime)

    def assetBalance(address: String, asset: String): AssetBalance =
      Await.result(async(n).assetBalance(address, asset), RequestAwaitTime)

    def assetsDetails(assetId: String): AssetInfo =
      Await.result(async(n).assetsDetails(assetId), RequestAwaitTime)

    def addressScriptInfo(address: String): AddressApiRoute.AddressScriptInfo =
      Await.result(async(n).scriptInfo(address), RequestAwaitTime)

    def assetsBalance(address: String): FullAssetsInfo =
      Await.result(async(n).assetsBalance(address), RequestAwaitTime)

    def issue(sourceAddress: String, name: String, description: String, quantity: Long, decimals: Byte, reissuable: Boolean, fee: Long): Transaction =
      Await.result(async(n).issue(sourceAddress, name, description, quantity, decimals, reissuable, fee), RequestAwaitTime)

    def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Transaction =
      Await.result(async(n).reissue(sourceAddress, assetId, quantity, reissuable, fee), RequestAwaitTime)

    def scriptCompile(code: String): CompiledScript =
      Await.result(async(n).scriptCompile(code), RequestAwaitTime)

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long): Transaction =
      Await.result(async(n).burn(sourceAddress, assetId, quantity, fee), RequestAwaitTime)

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long, version: String): Transaction =
      if (Option(version).nonEmpty) burnV2(sourceAddress, assetId, quantity, fee, version) else burn(sourceAddress, assetId, quantity, fee)

    def burnV2(sourceAddress: String, assetId: String, quantity: Long, fee: Long, version: String): Transaction = {
      signAndBroadcast(
        Json.obj("type" -> 6, "quantity" -> quantity, "assetId" -> assetId, "sender" -> sourceAddress, "fee" -> fee, "version" -> version))
    }

    def sponsorAsset(sourceAddress: String, assetId: String, baseFee: Long, fee: Long): Transaction =
      Await.result(async(n).sponsorAsset(sourceAddress, assetId, baseFee, fee), RequestAwaitTime)

    def cancelSponsorship(sourceAddress: String, assetId: String, fee: Long): Transaction =
      Await.result(async(n).cancelSponsorship(sourceAddress, assetId, fee), RequestAwaitTime)

    def sign(jsObject: JsObject): JsObject =
      Await.result(async(n).sign(jsObject), RequestAwaitTime)

    def createAlias(targetAddress: String, alias: String, fee: Long): Transaction =
      Await.result(async(n).createAlias(targetAddress, alias, fee), RequestAwaitTime)

    def aliasByAddress(targetAddress: String): Seq[String] =
      Await.result(async(n).aliasByAddress(targetAddress), RequestAwaitTime)

    def transfer(sourceAddress: String,
                 recipient: String,
                 amount: Long,
                 fee: Long,
                 assetId: Option[String] = None,
                 feeAssetId: Option[String] = None): Transaction =
      Await.result(async(n).transfer(sourceAddress, recipient, amount, fee, assetId, feeAssetId), RequestAwaitTime)

    def massTransfer(sourceAddress: String, transfers: List[Transfer], fee: Long, assetId: Option[String] = None): Transaction =
      Await.result(async(n).massTransfer(sourceAddress, transfers, fee, assetId), RequestAwaitTime)

    def lease(sourceAddress: String, recipient: String, leasingAmount: Long, leasingFee: Long): Transaction =
      Await.result(async(n).lease(sourceAddress, recipient, leasingAmount, leasingFee), RequestAwaitTime)

    def putData(sourceAddress: String, data: List[DataEntry[_]], fee: Long): Transaction =
      Await.result(async(n).putData(sourceAddress, data, fee), RequestAwaitTime)

    def getData(sourceAddress: String): List[DataEntry[_]] =
      Await.result(async(n).getData(sourceAddress), RequestAwaitTime)

    def getData(sourceAddress: String, key: String): DataEntry[_] =
      Await.result(async(n).getData(sourceAddress, key), RequestAwaitTime)

    def broadcastRequest[A: Writes](req: A): Transaction =
      Await.result(async(n).broadcastRequest(req), RequestAwaitTime)

    def activeLeases(sourceAddress: String): Seq[Transaction] =
      Await.result(async(n).activeLeases(sourceAddress), RequestAwaitTime)

    def cancelLease(sourceAddress: String, leaseId: String, fee: Long): Transaction =
      Await.result(async(n).cancelLease(sourceAddress, leaseId, fee), RequestAwaitTime)

    def signedBroadcast(tx: JsObject): Transaction =
      Await.result(async(n).signedBroadcast(tx), RequestAwaitTime)

    def signedIssue(tx: SignedIssueV1Request): Transaction =
      Await.result(async(n).signedIssue(tx), RequestAwaitTime)

    def ensureTxDoesntExist(txId: String): Unit =
      Await.result(async(n).ensureTxDoesntExist(txId), RequestAwaitTime)

    def createAddress(): String =
      Await.result(async(n).createAddress, RequestAwaitTime)

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): TransactionInfo =
      Await.result(async(n).waitForTransaction(txId), RequestAwaitTime)

    def signAndBroadcast(tx: JsObject): Transaction =
      Await.result(async(n).signAndBroadcast(tx), RequestAwaitTime)

    def waitForHeight(expectedHeight: Int, requestAwaitTime: FiniteDuration = RequestAwaitTime): Int =
      Await.result(async(n).waitForHeight(expectedHeight), requestAwaitTime)

    def debugMinerInfo(): Seq[State] =
      Await.result(async(n).debugMinerInfo(), RequestAwaitTime)

    def debugStateAt(height: Long): Map[String, Long] = Await.result(async(n).debugStateAt(height), RequestAwaitTime)

    def height: Int =
      Await.result(async(n).height, RequestAwaitTime)

    def rollback(to: Int, returnToUTX: Boolean = true): Unit =
      Await.result(async(n).rollback(to, returnToUTX), RequestAwaitTime)

    def getOrderBook(asset: String): OrderBookResponse =
      Await.result(async(n).getOrderBook(asset), RequestAwaitTime)

    def getOrderbookByPublicKey(publicKey: String, timestamp: Long, signature: ByteStr): Seq[OrderbookHistory] =
      Await.result(async(n).getOrderbookByPublicKey(publicKey, timestamp, signature), RequestAwaitTime)

    def getOrderbookByPublicKeyActive(publicKey: String, timestamp: Long, signature: ByteStr): Seq[OrderbookHistory] =
      Await.result(async(n).getOrderbookByPublicKeyActive(publicKey, timestamp, signature), RequestAwaitTime)

    def matcherGet(path: String,
                   f: RequestBuilder => RequestBuilder = identity,
                   statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                   waitForStatus: Boolean = false): Response =
      Await.result(async(n).matcherGet(path, f, statusCode, waitForStatus), RequestAwaitTime)

    def matcherGetStatusCode(path: String, statusCode: Int): MessageMatcherResponse =
      Await.result(async(n).matcherGetStatusCode(path, statusCode), RequestAwaitTime)

    def matcherPost[A: Writes](path: String, body: A): Response =
      Await.result(async(n).matcherPost(path, body), RequestAwaitTime)

    def placeOrder(order: Order): MatcherResponse =
      Await.result(async(n).placeOrder(order), RequestAwaitTime)

    def getOrderStatus(asset: String, orderId: String): MatcherStatusResponse =
      Await.result(async(n).getOrderStatus(asset, orderId), RequestAwaitTime)

    def waitOrderStatus(asset: String, orderId: String, expectedStatus: String, waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(n).waitOrderStatus(asset, orderId, expectedStatus), waitTime)

    def waitFor[A](desc: String)(f: async => A, cond: A => Boolean, retryInterval: FiniteDuration): A =
      Await.result(async(n).waitFor(desc)(z => Future(f(z))(scala.concurrent.ExecutionContext.Implicits.global), cond, retryInterval),
                   RequestAwaitTime)

    def getReservedBalance(publicKey: String, timestamp: Long, signature: ByteStr, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      Await.result(async(n).getReservedBalance(publicKey, timestamp, signature), waitTime)

    def expectIncorrectOrderPlacement(order: Order, expectedStatusCode: Int, expectedStatus: String): Boolean =
      Await.result(async(n).expectIncorrectOrderPlacement(order, expectedStatusCode, expectedStatus), 1.minute)

    def cancelOrder(amountAsset: String,
                    priceAsset: String,
                    request: CancelOrderRequest,
                    waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      Await.result(async(n).cancelOrder(amountAsset, priceAsset, request), waitTime)

  }

  implicit class NodesExtSync(nodes: Seq[Node]) {

    import com.wavesplatform.it.api.AsyncHttpApi.{NodesAsyncHttpApi => async}

    private val TxInBlockchainAwaitTime = 8 * nodes.head.blockDelay
    private val ConditionAwaitTime      = 5.minutes

    def waitForHeightAriseAndTxPresent(transactionId: String)(implicit pos: Position): Unit =
      Await.result(async(nodes).waitForHeightAriseAndTxPresent(transactionId), TxInBlockchainAwaitTime)

    def waitForHeightArise(): Unit =
      Await.result(async(nodes).waitForHeightArise(), TxInBlockchainAwaitTime)

    def waitForSameBlockHeadesAt(height: Int, retryInterval: FiniteDuration = 5.seconds): Boolean =
      Await.result(async(nodes).waitForSameBlockHeadesAt(height, retryInterval), ConditionAwaitTime)

    def waitFor[A](desc: String)(retryInterval: FiniteDuration)(request: Node => A, cond: Iterable[A] => Boolean): Boolean =
      Await.result(
        async(nodes).waitFor(desc)(retryInterval)((n: Node) => Future(request(n))(scala.concurrent.ExecutionContext.Implicits.global), cond),
        ConditionAwaitTime)
  }

}
