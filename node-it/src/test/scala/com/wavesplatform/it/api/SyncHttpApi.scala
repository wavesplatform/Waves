package com.wavesplatform.it.api

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeoutException

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.model.StatusCodes.BadRequest
import com.wavesplatform.account.{AddressOrAlias, AddressScheme, KeyPair}
import com.wavesplatform.api.http.RewardApiRoute.RewardStatus
import com.wavesplatform.api.http.assets.{SignedIssueV1Request, SignedIssueV2Request}
import com.wavesplatform.api.http.{AddressApiRoute, ApiError}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.api.{ActivationStatus, FeatureActivationStatus}
import com.wavesplatform.http.DebugMessage
import com.wavesplatform.it.Node
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, DataEntry, Portfolio}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV2, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import org.asynchttpclient.Response
import org.scalactic.source.Position
import org.scalatest.Matchers._
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

  case class GenericApiError(id: Int, message: String, statusCode: Int, json: JsObject)

  object GenericApiError {
    import play.api.libs.functional.syntax._
    import play.api.libs.json.Reads._
    import play.api.libs.json._

    def apply(id: Int, message: String, code: StatusCode, json: JsObject): GenericApiError =
      new GenericApiError(id, message, code.intValue(), json)

    implicit val genericApiErrorReads: Reads[GenericApiError] = (
      (JsPath \ "error").read[Int] and
        (JsPath \ "message").read[String] and
        JsPath.read[JsObject]
    )((id, message, json) => GenericApiError(id, message, StatusCodes.BadRequest.intValue, json))
  }

  case class AssertiveApiError(id: Int, message: String, code: StatusCode = StatusCodes.BadRequest, matchMessage: Boolean = false)

  implicit class ApiErrorOps(error: ApiError) {
    def assertive(matchMessage: Boolean = false): AssertiveApiError = AssertiveApiError(error.id, error.message, error.code, matchMessage)
  }

  def assertBadRequestAndResponse[R](f: => R, errorRegex: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
      Assertions.assert(statusCode == BadRequest.intValue && responseBody.replace("\n", "").matches(s".*$errorRegex.*"),
                        s"\nexpected '$errorRegex'\nactual '$responseBody'")
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail("Expecting bad request")
  }

  def assertBadRequestAndMessage[R](f: => R, errorMessage: String, expectedStatusCode: Int = BadRequest.intValue): Assertion =
    Try(f) match {
      case Failure(UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
        Assertions.assert(statusCode == expectedStatusCode && parse(responseBody).as[ErrorMessage].message.contains(errorMessage))
      case Failure(e) => Assertions.fail(e)
      case Success(s) => Assertions.fail(s"Expecting bad request but handle $s")
    }


  def assertApiErrorRaised[R](f: => R, expectedStatusCode: Int = StatusCodes.BadRequest.intValue): Assertion =
    assertApiError(f)(_ => Assertions.succeed)

  def assertApiError[R](f: => R, expectedError: AssertiveApiError): Assertion =
    assertApiError(f) { error =>
      error.id shouldBe expectedError.id
      error.statusCode shouldBe expectedError.code.intValue()
      if (expectedError.matchMessage)
        error.message should include regex expectedError.message
      else
        error.message shouldBe expectedError.message
    }

  def assertApiError[R](f: => R, expectedError: ApiError): Assertion =
    Try(f) match {
      case Failure(UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
        import play.api.libs.json._
        parse(responseBody).validate[JsObject] match {
          case JsSuccess(json, _) => (json - "trace") shouldBe expectedError.json
          case JsError(_)         => Assertions.fail(s"Expecting error: ${expectedError.json}, but handle $responseBody")
        }
        statusCode shouldBe expectedError.code.intValue()
      case Failure(e) => Assertions.fail(e)
      case Success(s) => Assertions.fail(s"Expecting error: $expectedError, but handle $s")
    }

  def assertApiError[R](f: => R)(check: GenericApiError => Assertion): Assertion =
    Try(f) match {
      case Failure(UnexpectedStatusCodeException(_, _, code, responseBody)) =>
        parse(responseBody).validate[GenericApiError] match {
          case JsSuccess(error, _) => check(error.copy(statusCode = code))
          case JsError(errors)     => Assertions.fail(errors.map { case (_, es) => es.mkString("(", ",", ")") }.mkString(","))
        }
      case Failure(e) => Assertions.fail(e)
      case Success(s) => Assertions.fail(s"Expecting error but handle $s")
    }

  val RequestAwaitTime: FiniteDuration = 50.seconds

  def sync[A](awaitable: Awaitable[A], atMost: Duration = RequestAwaitTime): A =
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

    def rewardStatus(height: Int): RewardStatus =
      sync(async(n).rewardStatus(height))

    def seed(address: String): String =
      sync(async(n).seed(address))

    def lastBlock: Block = sync(async(n).lastBlock)

    def lastBlockHeaders: BlockHeaders = sync(async(n).lastBlockHeaders)

    def blockHeadersAt(height: Int): BlockHeaders = sync(async(n).blockHeadersAt(height))

    def postForm(path: String, params: (String, String)*): Response =
      sync(async(n).postForm(path, params:_*))

    def postJson[A: Writes](path: String, body: A): Response =
      sync(async(n).postJson(path, body))

    def postJsonWithApiKey[A: Writes](path: String, body: A): Response =
      sync(async(n).postJsonWithApiKey(path, body))

    def getWithApiKey(path: String): Response =
      sync(async(n).getWithApiKey(path))

    def accountBalances(acc: String): (Long, Long) =
      sync(async(n).accountBalances(acc))

    def accountsBalances(height: Option[Int], accounts: Seq[String], asset: Option[String] = None): Seq[(String, Long)] =
      sync(async(n).accountsBalances(height, accounts, asset))

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

    def nftAssetsBalance(address: String, limit: Int): Seq[NFTAssetInfo] =
      sync(async(n).nftAssetsBalance(address, limit))

    def nftAssetsBalance(address: String, limit: Int, after: String): Seq[NFTAssetInfo] =
      sync(async(n).nftAssetsBalance(address, limit, after))

    def assetDistributionAtHeight(asset: String, height: Int, limit: Int, maybeAfter: Option[String] = None): AssetDistributionPage =
      sync(async(n).assetDistributionAtHeight(asset, height, limit, maybeAfter))

    def assetDistribution(asset: String): AssetDistribution =
      sync(async(n).assetDistribution(asset))

    def debugPortfoliosFor(address: String, considerUnspent: Boolean): Portfolio = sync(async(n).debugPortfoliosFor(address, considerUnspent))

    def broadcastIssue(
                        source: KeyPair,
                        name: String,
                        description: String,
                        quantity: Long,
                        decimals: Byte,
                        reissuable: Boolean,
                        fee: Long,
                        script: Option[String],
                        waitForTx: Boolean = false
                      ): Transaction = {
      val tx = IssueTransactionV2
        .selfSigned(
          chainId = AddressScheme.current.chainId,
          sender = source,
          name = name.getBytes(StandardCharsets.UTF_8),
          description = description.getBytes(StandardCharsets.UTF_8),
          quantity = quantity,
          decimals = decimals,
          reissuable = reissuable,
          script = script.map(x => Script.fromBase64String(x).explicitGet()),
          fee = fee,
          timestamp = System.currentTimeMillis()
        )
        .explicitGet()

      maybeWaitForTransaction(sync(async(n).broadcastRequest(tx.json())), wait = waitForTx)
    }

    def issue(
        sourceAddress: String,
        name: String,
        description: String,
        quantity: Long,
        decimals: Byte,
        reissuable: Boolean = true,
        fee: Long = 100000000,
        version: Byte = 2,
        script: Option[String] = None,
        waitForTx: Boolean = false
    ): Transaction = {
      maybeWaitForTransaction(sync(async(n).issue(sourceAddress, name, description, quantity, decimals, reissuable, fee, version, script)), waitForTx)
    }

    def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Transaction =
      sync(async(n).reissue(sourceAddress, assetId, quantity, reissuable, fee))

    def debugStateChanges(transactionId: String): DebugStateChanges = {
      sync(async(n).debugStateChanges(transactionId))
    }

    def debugStateChangesByAddress(address: String, limit: Int): Seq[DebugStateChanges] = {
      sync(async(n).debugStateChangesByAddress(address, limit))
    }

    def payment(sourceAddress: String, recipient: String, amount: Long, fee: Long): Transaction =
      sync(async(n).payment(sourceAddress, recipient, amount, fee))

    def transactionInfo(txId: String): TransactionInfo =
      sync(async(n).transactionInfo(txId))

    def transactionStatus(txIds: Seq[String]): Seq[TransactionStatus] =
      sync(async(n).transactionsStatus(txIds))

    def transactionsByAddress(address: String, limit: Int): Seq[TransactionInfo] =
      sync(async(n).transactionsByAddress(address, limit))

    def transactionsByAddress(address: String, limit: Int, after: String): Seq[TransactionInfo] =
      sync(async(n).transactionsByAddress(address, limit, after))

    def scriptCompile(code: String): CompiledScript =
      sync(async(n).scriptCompile(code))

    def scriptDecompile(code: String): DecompiledScript =
      sync(async(n).scriptDecompile(code))

    def getAddresses: Seq[String] = sync(async(n).getAddresses)

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long, version: Byte = 1, waitForTx: Boolean = false): Transaction =
      maybeWaitForTransaction(sync(async(n).burn(sourceAddress, assetId, quantity, fee, version)), waitForTx)

    def sponsorAsset(sourceAddress: String, assetId: String, baseFee: Long, fee: Long = 100000000, waitForTx: Boolean = false): Transaction = {
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

    def broadcastTransfer(
        source: KeyPair,
        recipient: String,
        amount: Long,
        fee: Long,
        assetId: Option[String],
        feeAssetId: Option[String],
        waitForTx: Boolean = false
    ): Transaction = {
      val tx = TransferTransactionV2
        .selfSigned(
          assetId = Asset.fromString(assetId),
          sender = source,
          recipient = AddressOrAlias.fromString(recipient).explicitGet(),
          amount = amount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = Asset.fromString(feeAssetId),
          feeAmount = fee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

      maybeWaitForTransaction(sync(async(n).broadcastRequest(tx.json())), wait = waitForTx)
    }

    def transfer(
        sourceAddress: String,
        recipient: String,
        amount: Long,
        fee: Long,
        assetId: Option[String] = None,
        feeAssetId: Option[String] = None,
        version: Byte = 2,
        waitForTx: Boolean = false
    ): Transaction = {
      maybeWaitForTransaction(sync(async(n).transfer(sourceAddress, recipient, amount, fee, assetId, feeAssetId, version)), waitForTx)
    }

    def massTransfer(
        sourceAddress: String,
        transfers: List[Transfer],
        fee: Long,
        assetId: Option[String] = None,
        waitForTx: Boolean = false
    ): Transaction = {
      maybeWaitForTransaction(sync(async(n).massTransfer(sourceAddress, transfers, fee, assetId)), waitForTx)
    }

    def broadcastLease(source: KeyPair, recipient: String, leasingAmount: Long, leasingFee: Long, waitForTx: Boolean = false): Transaction = {
      val tx = LeaseTransactionV2
        .selfSigned(
          sender = source,
          amount = leasingAmount,
          fee = leasingFee,
          timestamp = System.currentTimeMillis(),
          recipient = AddressOrAlias.fromString(recipient).explicitGet()
        )
        .explicitGet()

      maybeWaitForTransaction(sync(async(n).broadcastRequest(tx.json())), wait = waitForTx)
    }

    def lease(
        sourceAddress: String,
        recipient: String,
        leasingAmount: Long,
        leasingFee: Long,
        version: Byte = 1,
        waitForTx: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).lease(sourceAddress, recipient, leasingAmount, leasingFee, version)), waitForTx)

    def putData(sourceAddress: String, data: List[DataEntry[_]], fee: Long, waitForTx: Boolean = false): Transaction =
      maybeWaitForTransaction(sync(async(n).putData(sourceAddress, data, fee)), waitForTx)

    def getData(sourceAddress: String): List[DataEntry[_]] =
      sync(async(n).getData(sourceAddress))

    def getData(sourceAddress: String, regexp: String): List[DataEntry[_]] =
      sync(async(n).getData(sourceAddress, regexp))

    def getDataByKey(sourceAddress: String, key: String): DataEntry[_] =
      sync(async(n).getDataByKey(sourceAddress, key))

    def getDataList(sourceAddress: String, keys: String*): Seq[DataEntry[_]] =
      sync(async(n).getDataList(sourceAddress, keys:_*))

    def getDataListJson(sourceAddress: String, keys: String*): Seq[DataEntry[_]] =
      sync(async(n).getDataListJson(sourceAddress, keys:_*))

    def getDataListPost(sourceAddress: String, keys: String*): Seq[DataEntry[_]] =
      sync(async(n).getDataListPost(sourceAddress, keys:_*))

    def broadcastRequest[A: Writes](req: A): Transaction =
      sync(async(n).broadcastRequest(req))

    def activeLeases(sourceAddress: String): Seq[Transaction] =
      sync(async(n).activeLeases(sourceAddress))

    def broadcastCancelLease(source: KeyPair, leaseId: String, fee: Long, waitForTx: Boolean = false): Transaction = {
      val tx = LeaseCancelTransactionV2
        .selfSigned(
          chainId = AddressScheme.current.chainId,
          sender = source,
          leaseId = ByteStr.decodeBase58(leaseId).get,
          fee = fee,
          timestamp = System.currentTimeMillis()
        )
        .explicitGet()

      maybeWaitForTransaction(sync(async(n).broadcastRequest(tx.json())), wait = waitForTx)
    }

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

    def signAndTraceBroadcast(tx: JsValue, waitForTx: Boolean = false): (Transaction, JsValue) = {
      sync(async(n).signAndTraceBroadcast(tx)) match {
        case (tx, js) => maybeWaitForTransaction(tx, waitForTx) -> js
      }
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

    def blockSeqByAddress(address: String, from: Int, to: Int): Seq[Block] = sync(async(n).blockSeqByAddress(address, from, to))

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

    def invokeScript(
        caller: String,
        dappAddress: String,
        func: Option[String],
        args: List[Terms.EXPR] = List.empty,
        payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty,
        fee: Long = 500000,
        feeAssetId: Option[String] = None,
        version: Byte = 1,
        waitForTx: Boolean = false
    ): (Transaction, JsValue) = {
      sync(async(n).invokeScript(caller, dappAddress, func, args, payment, fee, feeAssetId, version)) match {
        case (tx, js) => maybeWaitForTransaction(tx, waitForTx) -> js
      }
    }

    def waitForUtxIncreased(fromSize: Int): Int = sync(async(n).waitForUtxIncreased(fromSize))

    def featureActivationStatus(featureNum: Short): FeatureActivationStatus =
      activationStatus.features.find(_.id == featureNum).get

  }

  implicit class NodesExtSync(nodes: Seq[Node]) {

    import com.wavesplatform.it.api.AsyncHttpApi.{NodesAsyncHttpApi => async}

    private val TxInBlockchainAwaitTime = 8 * nodes.head.blockDelay
    private val ConditionAwaitTime      = 5.minutes

    private[this] def withTxIdMessage[T](transactionId: String)(f: => T): T =
      try f
      catch { case NonFatal(cause) => throw new RuntimeException(s"Error awaiting transaction: $transactionId", cause) }

    def height(implicit pos: Position): Seq[Int] =
      sync(async(nodes).height, TxInBlockchainAwaitTime)

    def waitForHeightAriseAndTxPresent(transactionId: String)(implicit pos: Position): Unit =
      withTxIdMessage(transactionId)(sync(async(nodes).waitForHeightAriseAndTxPresent(transactionId), TxInBlockchainAwaitTime))

    def waitForTransaction(transactionId: String)(implicit pos: Position): TransactionInfo =
      withTxIdMessage(transactionId)(sync(async(nodes).waitForTransaction(transactionId), TxInBlockchainAwaitTime))

    def waitForHeightArise(): Int =
      sync(async(nodes).waitForHeightArise(), TxInBlockchainAwaitTime)

    def waitForSameBlockHeadersAt(
        height: Int,
        retryInterval: FiniteDuration = 5.seconds,
        conditionAwaitTime: FiniteDuration = ConditionAwaitTime
    ): Boolean =
      sync(async(nodes).waitForSameBlockHeadersAt(height, retryInterval), conditionAwaitTime)

    def waitFor[A](desc: String)(retryInterval: FiniteDuration)(request: Node => A, cond: Iterable[A] => Boolean): Boolean =
      sync(
        async(nodes).waitFor(desc)(retryInterval)((n: Node) => Future(request(n))(scala.concurrent.ExecutionContext.Implicits.global), cond),
        ConditionAwaitTime
      )

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
