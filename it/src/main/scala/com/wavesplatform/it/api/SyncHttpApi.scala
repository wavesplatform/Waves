package com.wavesplatform.it.api

import java.net.InetSocketAddress
import java.util.concurrent.TimeoutException

import akka.http.scaladsl.model.StatusCodes.{BadRequest, NotFound}
import com.wavesplatform.api.http.AddressApiRoute
import com.wavesplatform.api.http.assets.{SignedIssueV1Request, SignedIssueV2Request}
import com.wavesplatform.features.api.{ActivationStatus, FeatureActivationStatus}
import com.wavesplatform.http.DebugMessage
import com.wavesplatform.it.Node
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, DataEntry, Portfolio}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import org.asynchttpclient.Response
import org.scalactic.source.Position
import org.scalatest.{Assertion, Assertions, Matchers}
import play.api.libs.json.Json.parse
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable, Future}
import scala.util._
import scala.util.control.NonFatal

object SyncHttpApi extends Assertions {
  case class ErrorMessage(error: Int, message: String)
  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

  case class NotFoundErrorMessage(status: String, details: String)

  object NotFoundErrorMessage {
    implicit val format: Format[NotFoundErrorMessage] = Json.format
  }

  def assertBadRequest[R](f: => R, expectedStatusCode: Int = 400): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, _, statusCode, _)) => Assertions.assert(statusCode == expectedStatusCode)
    case Failure(e)                                                  => Assertions.fail(e)
    case _                                                           => Assertions.fail("Expecting bad request")
  }

  def assertBadRequestAndResponse[R](f: => R, errorRegex: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
      Assertions.assert(statusCode == BadRequest.intValue && responseBody.replace("\n", "").matches(s".*$errorRegex.*"),
                        s"\nexpected '$errorRegex'\nactual '$responseBody'")
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail("Expecting bad request")
  }

  def assertBadRequestAndMessage[R](f: => R, errorMessage: String, expectedStatusCode: Int = BadRequest.intValue): Assertion = Try(f) match {
    case Failure(e @ UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
      Assertions.assert(statusCode == expectedStatusCode && parse(responseBody).as[ErrorMessage].message.contains(errorMessage))
    case Failure(e) => Assertions.fail(e)
    case Success(s) => Assertions.fail(s"Expecting bad request but handle $s")
  }

  def assertNotFoundAndMessage[R](f: => R, errorMessage: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
      Assertions.assert(statusCode == NotFound.intValue && parse(responseBody).as[NotFoundErrorMessage].details.contains(errorMessage))
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail(s"Expecting not found error")
  }

  val RequestAwaitTime = 50.seconds

  def sync[A](awaitable: Awaitable[A], atMost: Duration = RequestAwaitTime) =
    try Await.result(awaitable, atMost)
    catch {
      case usce: UnexpectedStatusCodeException => throw usce
      case te: TimeoutException                => throw te
      case NonFatal(cause)                     => throw new Exception(cause)
    }

  implicit class NodeExtSync(n: Node) extends Assertions with Matchers {
    import com.wavesplatform.it.api.AsyncHttpApi.{NodeAsyncHttpApi => async}

    private def maybeWaitForTransaction(tx: Transaction, wait: Boolean): Transaction = {
      if (wait) waitForTransaction(tx.id)
      tx
    }

    def get(path: String): Response =
      sync(async(n).get(path))

    def utx: Seq[Transaction] = sync(async(n).utx)

    def utxSize: Int = sync(async(n).utxSize)

    def printDebugMessage(db: DebugMessage): Response =
      sync(async(n).printDebugMessage(db))

    def activationStatus: ActivationStatus =
      sync(async(n).activationStatus)

    def seed(address: String): String =
      sync(async(n).seed(address))

    def lastBlock: Block = sync(async(n).lastBlock)

    def lastBlockHeaders: BlockHeaders = sync(async(n).lastBlockHeaders)

    def blockHeadersAt(height: Int): BlockHeaders = sync(async(n).blockHeadersAt(height))

    def postJson[A: Writes](path: String, body: A): Response =
      sync(async(n).postJson(path, body))

    def postJsonWithApiKey[A: Writes](path: String, body: A): Response =
      sync(async(n).postJsonWithApiKey(path, body))

    def getWithApiKey(path: String): Response =
      sync(async(n).getWithApiKey(path))

    def accountBalances(acc: String): (Long, Long) =
      sync(async(n).accountBalances(acc))

    def balanceDetails(acc: String): BalanceDetails = sync(async(n).balanceDetails(acc))

    def assertBalances(acc: String, balance: Long)(implicit pos: Position): Unit =
      sync(async(n).assertBalances(acc, balance, effectiveBalance = balance))

    def assertBalances(acc: String, balance: Long, effectiveBalance: Long)(implicit pos: Position): Unit =
      sync(async(n).assertBalances(acc, balance, effectiveBalance))

    def assertAssetBalance(acc: String, assetIdString: String, balance: Long)(implicit pos: Position): Unit =
      sync(async(n).assertAssetBalance(acc, assetIdString, balance))

    def assetBalance(address: String, asset: String): AssetBalance =
      sync(async(n).assetBalance(address, asset))

    def assetsDetails(assetId: String, fullInfo: Boolean = false): AssetInfo =
      sync(async(n).assetsDetails(assetId, fullInfo))

    def addressScriptInfo(address: String): AddressApiRoute.AddressScriptInfo =
      sync(async(n).scriptInfo(address))

    def assetsBalance(address: String): FullAssetsInfo =
      sync(async(n).assetsBalance(address))

    def assetDistributionAtHeight(asset: String, height: Int, limit: Int, maybeAfter: Option[String] = None): AssetDistributionPage =
      sync(async(n).assetDistributionAtHeight(asset, height, limit, maybeAfter))

    def assetDistribution(asset: String): AssetDistribution =
      sync(async(n).assetDistribution(asset))

    def debugPortfoliosFor(address: String, considerUnspent: Boolean): Portfolio = sync(async(n).debugPortfoliosFor(address, considerUnspent))

    def issue(sourceAddress: String,
              name: String,
              description: String,
              quantity: Long,
              decimals: Byte,
              reissuable: Boolean,
              fee: Long,
              version: Byte = 2,
              script: Option[String] = None,
              waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).issue(sourceAddress, name, description, quantity, decimals, reissuable, fee, version, script)), waitForTx)
    }

    def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Transaction =
      sync(async(n).reissue(sourceAddress, assetId, quantity, reissuable, fee))

    def payment(sourceAddress: String, recipient: String, amount: Long, fee: Long): Transaction =
      sync(async(n).payment(sourceAddress, recipient, amount, fee))

    def transactionInfo(txId: String): TransactionInfo =
      sync(async(n).transactionInfo(txId))

    def transactionsByAddress(address: String, limit: Int): Seq[Seq[TransactionInfo]] =
      sync(async(n).transactionsByAddress(address, limit))

    def transactionsByAddress(address: String, limit: Int, after: String): Seq[Seq[TransactionInfo]] =
      sync(async(n).transactionsByAddress(address, limit, after))

    def scriptCompile(code: String): CompiledScript =
      sync(async(n).scriptCompile(code))

    def getAddresses: Seq[String] = sync(async(n).getAddresses)

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long, version: Byte = 1, waitForTx: Boolean = false): Transaction =
      maybeWaitForTransaction(sync(async(n).burn(sourceAddress, assetId, quantity, fee, version)), waitForTx)

    def sponsorAsset(sourceAddress: String, assetId: String, baseFee: Long, fee: Long, waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).sponsorAsset(sourceAddress, assetId, baseFee, fee)), waitForTx)
    }

    def cancelSponsorship(sourceAddress: String, assetId: String, fee: Long): Transaction =
      sync(async(n).cancelSponsorship(sourceAddress, assetId, fee))

    def sign(json: JsValue): JsObject =
      sync(async(n).sign(json))

    def createAlias(targetAddress: String, alias: String, fee: Long, version: Byte = 2): Transaction =
      sync(async(n).createAlias(targetAddress, alias, fee, version))

    def aliasByAddress(targetAddress: String): Seq[String] =
      sync(async(n).aliasByAddress(targetAddress))

    def transfer(sourceAddress: String,
                 recipient: String,
                 amount: Long,
                 fee: Long,
                 assetId: Option[String] = None,
                 feeAssetId: Option[String] = None,
                 version: Byte = 1,
                 waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).transfer(sourceAddress, recipient, amount, fee, assetId, feeAssetId, version)), waitForTx)
    }

    def massTransfer(sourceAddress: String,
                     transfers: List[Transfer],
                     fee: Long,
                     assetId: Option[String] = None,
                     waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).massTransfer(sourceAddress, transfers, fee, assetId)), waitForTx)
    }

    def lease(sourceAddress: String,
              recipient: String,
              leasingAmount: Long,
              leasingFee: Long,
              version: Byte = 1,
              waitForTx: Boolean = false): Transaction =
      maybeWaitForTransaction(sync(async(n).lease(sourceAddress, recipient, leasingAmount, leasingFee, version)), waitForTx)

    def putData(sourceAddress: String, data: List[DataEntry[_]], fee: Long): Transaction =
      sync(async(n).putData(sourceAddress, data, fee))

    def getData(sourceAddress: String): List[DataEntry[_]] =
      sync(async(n).getData(sourceAddress))

    def getData(sourceAddress: String, key: String): DataEntry[_] =
      sync(async(n).getData(sourceAddress, key))

    def broadcastRequest[A: Writes](req: A): Transaction =
      sync(async(n).broadcastRequest(req))

    def activeLeases(sourceAddress: String): Seq[Transaction] =
      sync(async(n).activeLeases(sourceAddress))

    def cancelLease(sourceAddress: String, leaseId: String, fee: Long, version: Byte = 1): Transaction =
      sync(async(n).cancelLease(sourceAddress, leaseId, fee))

    def expectSignedBroadcastRejected(json: JsValue): Int = sync(async(n).expectSignedBroadcastRejected(json))

    def signedBroadcast(tx: JsValue, waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).signedBroadcast(tx)), waitForTx)
    }

    def signedIssue(tx: SignedIssueV1Request): Transaction =
      sync(async(n).signedIssue(tx))

    def signedIssue(tx: SignedIssueV2Request): Transaction =
      sync(async(n).signedIssue(tx))

    def ensureTxDoesntExist(txId: String): Unit =
      sync(async(n).ensureTxDoesntExist(txId))

    def createAddress(): String =
      sync(async(n).createAddress)

    def rawTransactionInfo(txId: String): JsValue =
      sync(async(n).rawTransactionInfo(txId))

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): TransactionInfo =
      sync(async(n).waitForTransaction(txId))

    def signAndBroadcast(tx: JsValue, waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).signAndBroadcast(tx)), waitForTx)
    }

    def waitForHeight(expectedHeight: Int, requestAwaitTime: FiniteDuration = RequestAwaitTime): Int =
      sync(async(n).waitForHeight(expectedHeight), requestAwaitTime)

    def blacklist(address: InetSocketAddress): Unit =
      sync(async(n).blacklist(address))

    def debugMinerInfo(): Seq[State] =
      sync(async(n).debugMinerInfo())

    def transactionSerializer(body: JsObject): TransactionSerialize = sync(async(n).transactionSerializer(body))

    def debugStateAt(height: Long): Map[String, Long] = sync(async(n).debugStateAt(height))

    def height: Int =
      sync(async(n).height)

    def blockAt(height: Int): Block = sync(async(n).blockAt(height))

    def blockSeq(fromHeight: Int, toHeight: Int): Seq[Block] = sync(async(n).blockSeq(fromHeight, toHeight))

    def blockHeadersSeq(fromHeight: Int, toHeight: Int): Seq[BlockHeaders] = sync(async(n).blockHeadersSeq(fromHeight, toHeight))

    def rollback(to: Int, returnToUTX: Boolean = true): Unit =
      sync(async(n).rollback(to, returnToUTX))

    def findTransactionInfo(txId: String): Option[TransactionInfo] = sync(async(n).findTransactionInfo(txId))

    def connectedPeers: Seq[Peer] = (Json.parse(get("/peers/connected").getResponseBody) \ "peers").as[Seq[Peer]]

    def calculateFee(tx: JsObject): FeeInfo =
      sync(async(n).calculateFee(tx))

    def blacklistedPeers: Seq[BlacklistedPeer] =
      sync(async(n).blacklistedPeers)

    def waitFor[A](desc: String)(f: Node => A, cond: A => Boolean, retryInterval: FiniteDuration): A =
      sync(async(n).waitFor[A](desc)(x => Future.successful(f(x.n)), cond, retryInterval), 5.minutes)

    def waitForBlackList(blackList: Int): Seq[BlacklistedPeer] =
      sync(async(n).waitForBlackList(blackList))

    def status(): Status =
      sync(async(n).status)

    def waitForPeers(targetPeersCount: Int, requestAwaitTime: FiniteDuration = RequestAwaitTime): Seq[Peer] =
      sync(async(n).waitForPeers(targetPeersCount), requestAwaitTime)

    def connect(address: InetSocketAddress): Unit =
      sync(async(n).connect(address))

    def setScript(sender: String, script: Option[String] = None, fee: Long = 1000000, waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).setScript(sender, script, fee)), waitForTx)
    }

    def setAssetScript(assetId: String, sender: String, fee: Long, script: Option[String] = None, waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).setAssetScript(assetId, sender, fee, script)), waitForTx)
    }

    def invokeScript(caller: String,
                     dappAddress: String,
                     func: String,
                     args: List[Terms.EXPR] = List.empty,
                     payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty,
                     fee: Long = 500000,
                     feeAssetId: Option[String] = None,
                     version: Byte = 1,
                     waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).invokeScript(caller, dappAddress, func, args, payment, fee, feeAssetId, version)), waitForTx)
    }

    def waitForUtxIncreased(fromSize: Int): Int = sync(async(n).waitForUtxIncreased(fromSize))

    def featureActivationStatus(featureNum: Short): FeatureActivationStatus =
      activationStatus.features.find(_.id == featureNum).get
  }

  implicit class NodesExtSync(nodes: Seq[Node]) {

    import com.wavesplatform.it.api.AsyncHttpApi.{NodesAsyncHttpApi => async}

    private val TxInBlockchainAwaitTime = 8 * nodes.head.blockDelay
    private val ConditionAwaitTime      = 5.minutes

    def height(implicit pos: Position): Seq[Int] =
      sync(async(nodes).height, TxInBlockchainAwaitTime)

    def waitForHeightAriseAndTxPresent(transactionId: String)(implicit pos: Position): Unit =
      sync(async(nodes).waitForHeightAriseAndTxPresent(transactionId), TxInBlockchainAwaitTime)

    def waitForTransaction(transactionId: String)(implicit pos: Position): TransactionInfo =
      sync(async(nodes).waitForTransaction(transactionId), TxInBlockchainAwaitTime)

    def waitForHeightArise(): Int =
      sync(async(nodes).waitForHeightArise(), TxInBlockchainAwaitTime)

    def waitForSameBlockHeadesAt(height: Int,
                                 retryInterval: FiniteDuration = 5.seconds,
                                 conditionAwaitTime: FiniteDuration = ConditionAwaitTime): Boolean =
      sync(async(nodes).waitForSameBlockHeadesAt(height, retryInterval), conditionAwaitTime)

    def waitFor[A](desc: String)(retryInterval: FiniteDuration)(request: Node => A, cond: Iterable[A] => Boolean): Boolean =
      sync(async(nodes).waitFor(desc)(retryInterval)((n: Node) => Future(request(n))(scala.concurrent.ExecutionContext.Implicits.global), cond),
           ConditionAwaitTime)

    def rollback(height: Int, returnToUTX: Boolean = true): Unit = {
      sync(
        Future.traverse(nodes) { node =>
          com.wavesplatform.it.api.AsyncHttpApi.NodeAsyncHttpApi(node).rollback(height, returnToUTX)
        },
        ConditionAwaitTime
      )
    }

    def waitForHeight(height: Int): Unit = {
      sync(
        Future.traverse(nodes) { node =>
          com.wavesplatform.it.api.AsyncHttpApi.NodeAsyncHttpApi(node).waitForHeight(height)
        },
        ConditionAwaitTime
      )
    }
  }

}
