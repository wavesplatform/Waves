package com.wavesplatform.it.api

import java.net.InetSocketAddress
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.account.{AddressOrAlias, KeyPair, SeedKeyPair}
import com.wavesplatform.api.http.RewardApiRoute.RewardStatus
import com.wavesplatform.api.http.requests.IssueRequest
import com.wavesplatform.api.http.{ApiError, DebugMessage}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.api.{ActivationStatus, FeatureActivationStatus}
import com.wavesplatform.it.Node
import com.wavesplatform.it.sync.*
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.{AssetDistributionPage, DataEntry}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, TxExchangeAmount, TxExchangePrice, TxVersion}
import io.grpc.Status.Code
import org.asynchttpclient.Response
import org.scalactic.source.Position
import org.scalatest.{Assertion, Assertions, matchers}
import play.api.libs.json.*
import play.api.libs.json.Json.parse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Awaitable, Future}
import scala.util.*
import scala.util.control.NonFatal

object SyncHttpApi extends Assertions with matchers.should.Matchers {
  case class ApiCallException(cause: Throwable) extends Exception("Error in API call", cause)
  case class ErrorMessage(error: Int, message: String)
  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

  case class GenericApiError(id: Int, message: String, statusCode: Int, json: JsObject)

  object GenericApiError {
    import play.api.libs.functional.syntax.*
    import play.api.libs.json.*
    import play.api.libs.json.Reads.*

    def apply(id: Int, message: String, code: StatusCode, json: JsObject): GenericApiError =
      new GenericApiError(id, message, code.intValue(), json)

    implicit val genericApiErrorReads: Reads[GenericApiError] = (
      (JsPath \ "error").read[Int] and
        (JsPath \ "message").read[String] and
        JsPath.read[JsObject]
    )((id, message, json) => GenericApiError(id, message, StatusCodes.BadRequest.intValue, json))
  }

  /** @param id
    *   Expected API error code
    * @param message
    *   Expected API error full message or regex template
    * @param code
    *   Expected HTTP status code, 400/Bad Request by default
    * @param matchMessage
    *   When true, uses `message` as regular expression to find it in response. When false, fully tests `message` equality with received error
    *   message.
    */
  case class AssertiveApiError(id: Int, message: String, code: StatusCode = StatusCodes.BadRequest, matchMessage: Boolean = false)

  implicit class ApiErrorOps(error: ApiError) {
    def assertive(matchMessage: Boolean = false): AssertiveApiError = AssertiveApiError(error.id, error.message, error.code, matchMessage)
    def assertiveRegex: AssertiveApiError                           = assertive(matchMessage = true)
  }

  def assertBadRequestAndResponse[R](f: => R, errorRegex: String): Assertion = Try(f) match {
    case Failure(ApiCallException(UnexpectedStatusCodeException(_, _, statusCode, responseBody))) =>
      Assertions.assert(
        statusCode == BadRequest.intValue && responseBody.replace("\n", "").matches(s".*$errorRegex.*"),
        s"\nexpected '$errorRegex'\nactual '$responseBody'"
      )
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail("Expecting bad request")
  }

  def assertBadRequestAndMessage[R](f: => R, errorMessage: String, expectedStatusCode: Int = BadRequest.intValue): Assertion =
    Try(f) match {
      case Failure(ApiCallException(UnexpectedStatusCodeException(_, _, statusCode, responseBody))) =>
        Assertions.assert(statusCode == expectedStatusCode, s"Status code not match: $statusCode")
        val message1 = parse(responseBody).as[ErrorMessage].message
        Assertions.assert(message1.contains(errorMessage), s"Message not match: $message1")
      case Failure(e) =>
        Assertions.fail("Unexpected exception", new Exception(e.toString, e))
      case Success(s) => Assertions.fail(s"Expecting bad request but handle $s")
    }

  def assertGrpcError[R](f: => R, errorRegex: String, expectedCode: Code): Assertion = Try(f) match {
    case Failure(GrpcStatusRuntimeException(status, _)) =>
      Assertions.assert(
        status.getCode == expectedCode
          && status.getDescription.matches(s".*$errorRegex.*"),
        s"\nexpected '$errorRegex'\nactual '${status.getDescription}'"
      )
    case Failure(e) => Assertions.fail(e)
    case Success(s) => Assertions.fail(s"Expecting bad request but handle $s")
  }

  def assertApiErrorRaised[R](f: => R): Assertion =
    assertApiError(f)(_ => Assertions.succeed)

  def assertApiError[R](f: => R, expectedError: AssertiveApiError): Assertion =
    assertApiError(f) { error =>
      if (expectedError.matchMessage)
        error.message should include regex expectedError.message
      else
        error.message shouldBe expectedError.message

      error.statusCode shouldBe expectedError.code.intValue()
      error.id shouldBe expectedError.id
    }

  def assertApiError[R](f: => R, expectedError: ApiError): Assertion =
    Try(f) match {
      case Failure(ApiCallException(UnexpectedStatusCodeException(_, _, statusCode, responseBody))) =>
        import play.api.libs.json.*
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
      case Failure(ApiCallException(UnexpectedStatusCodeException(_, _, code, responseBody))) =>
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
      case NonFatal(cause) => throw ApiCallException(cause)
    }

  // noinspection ScalaStyle
  implicit class NodeExtSync(n: Node) extends Assertions with matchers.should.Matchers {
    import com.wavesplatform.it.api.AsyncHttpApi.NodeAsyncHttpApi as async

    private def maybeWaitForTransaction(tx: Transaction, wait: Boolean): Transaction = {
      if (wait) waitForTransaction(tx.id)
      tx
    }

    def get(path: String): Response =
      sync(async(n).get(path))

    def getWithCustomHeader(path: String, headerName: String = "Accept", headerValue: String, withApiKey: Boolean = false): Response =
      sync(async(n).getWithCustomHeader(path, headerName, headerValue, withApiKey))

    def utx(amountsAsStrings: Boolean = false): Seq[Transaction] = sync(async(n).utx(amountsAsStrings))

    def utxById(txId: String, amountsAsStrings: Boolean = false): Transaction = {
      sync(async(n).utxById(txId, amountsAsStrings))
    }

    def utxSize: Int = sync(async(n).utxSize)

    def printDebugMessage(db: DebugMessage): Response =
      sync(async(n).printDebugMessage(db))

    def activationStatus: ActivationStatus =
      sync(async(n).activationStatus)

    def rewardStatus(height: Option[Int] = None, amountsAsStrings: Boolean = false): RewardStatus =
      sync(async(n).rewardStatus(height, amountsAsStrings))

    def seed(address: String): String =
      sync(async(n).seed(address))

    def lastBlock(amountsAsStrings: Boolean = false): Block = sync(async(n).lastBlock(amountsAsStrings))

    def blockById(id: String, amountsAsStrings: Boolean = false): Block = sync(async(n).blockById(id, amountsAsStrings))

    def lastBlockHeader(amountsAsStrings: Boolean = false): BlockHeader = sync(async(n).lastBlockHeader(amountsAsStrings))

    def blockHeadersAt(height: Int, amountsAsStrings: Boolean = false): BlockHeader = sync(async(n).blockHeadersAt(height, amountsAsStrings))

    def blockHeaderForId(id: String, amountsAsStrings: Boolean = false): BlockHeader = sync(async(n).blockHeaderForId(id, amountsAsStrings))

    def postForm(path: String, params: (String, String)*): Response =
      sync(async(n).postForm(path, params*))

    def postJson[A: Writes](path: String, body: A): Response =
      sync(async(n).postJson(path, body))

    def postJsonWithApiKey[A: Writes](path: String, body: A): Response =
      sync(async(n).postJsonWithApiKey(path, body))

    def postJsObjectWithCustomHeader(path: String, body: JsValue, headerName: String = "Accept", headerValue: String): Response =
      sync(async(n).postJsObjectWithCustomHeader(path, body, headerName, headerValue))

    def getWithApiKey(path: String): Response =
      sync(async(n).getWithApiKey(path))

    def accountBalances(acc: String): (Long, Long) =
      sync(async(n).accountBalances(acc))

    def balanceAtHeight(address: String, height: Int): Long =
      sync(async(n).balanceAtHeight(address, height))

    def accountsBalances(height: Option[Int], accounts: Seq[String], asset: Option[String] = None): Seq[(String, Long)] =
      sync(async(n).accountsBalances(height, accounts, asset))

    def balance(address: String, confirmations: Option[Int] = None, amountsAsStrings: Boolean = false): Balance =
      sync(async(n).balance(address, confirmations, amountsAsStrings))

    def effectiveBalance(address: String, confirmations: Option[Int] = None, amountsAsStrings: Boolean = false): Balance =
      sync(async(n).effectiveBalance(address, confirmations, amountsAsStrings))

    def balanceDetails(acc: String, amountsAsStrings: Boolean = false): BalanceDetails = sync(async(n).balanceDetails(acc, amountsAsStrings))

    def assertBalances(acc: String, balance: Long)(implicit pos: Position): Unit =
      sync(async(n).assertBalances(acc, balance, effectiveBalance = balance))

    def assertBalances(acc: String, balance: Long, effectiveBalance: Long)(implicit pos: Position): Unit =
      sync(async(n).assertBalances(acc, balance, effectiveBalance))

    def assertAssetBalance(acc: String, assetIdString: String, balance: Long)(implicit pos: Position): Unit =
      sync(async(n).assertAssetBalance(acc, assetIdString, balance))

    def assetBalance(address: String, asset: String, amountsAsStrings: Boolean = false): AssetBalance =
      sync(async(n).assetBalance(address, asset, amountsAsStrings))

    def assetsDetails(assetId: String, fullInfo: Boolean = false, amountsAsStrings: Boolean = false): AssetInfo =
      sync(async(n).assetsDetails(assetId, fullInfo, amountsAsStrings))

    def addressScriptInfo(address: String): AddressScriptInfo =
      sync(async(n).scriptInfo(address))

    def assetsBalance(address: String, amountsAsStrings: Boolean = false): FullAssetsInfo =
      sync(async(n).assetsBalance(address, amountsAsStrings))

    def nftList(address: String, limit: Int, maybeAfter: Option[String] = None, amountsAsStrings: Boolean = false): Seq[NFTAssetInfo] =
      sync(async(n).nftList(address, limit, maybeAfter, amountsAsStrings))

    def assetDistributionAtHeight(
        asset: String,
        height: Int,
        limit: Int,
        maybeAfter: Option[String] = None,
        amountsAsStrings: Boolean = false
    ): AssetDistributionPage =
      sync(async(n).assetDistributionAtHeight(asset, height, limit, maybeAfter, amountsAsStrings))

    def broadcastIssue(
        source: KeyPair,
        name: String,
        description: String,
        quantity: Long,
        decimals: Byte,
        reissuable: Boolean,
        fee: Long = issueFee,
        version: TxVersion = TxVersion.V2,
        script: Option[String],
        waitForTx: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).issue(source, name, description, quantity, decimals, reissuable, fee, version, script)), wait = waitForTx)

    def issue(
        source: KeyPair,
        name: String = "Asset",
        description: String = "",
        quantity: Long = 1000000000,
        decimals: Byte = 2,
        reissuable: Boolean = true,
        fee: Long = issueFee,
        version: TxVersion = TxVersion.V2,
        script: Option[String] = None,
        waitForTx: Boolean = false
    ): Transaction =
      broadcastIssue(source, name, description, quantity, decimals, reissuable, fee, version, script, waitForTx)

    def reissue(
        sender: KeyPair,
        assetId: String,
        quantity: Long,
        reissuable: Boolean,
        fee: Long = reissueFee,
        version: Byte = 1,
        waitForTx: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).reissue(sender, assetId, quantity, reissuable, fee, version)), waitForTx)

    def stateChanges(transactionId: String, amountsAsStrings: Boolean = false): StateChanges = {
      sync(async(n).stateChanges(transactionId, amountsAsStrings))
    }

    def payment(sourceAddress: String, recipient: String, amount: Long, fee: Long): Transaction =
      sync(async(n).payment(sourceAddress, recipient, amount, fee))

    def transactionInfo[A: Reads](txId: String, amountsAsStrings: Boolean = false): A =
      sync(async(n).transactionInfo[A](txId, amountsAsStrings))

    def transactionStatus(txIds: Seq[String]): Seq[TransactionStatus] =
      sync(async(n).transactionsStatus(txIds))

    def transactionStatus(txId: String): TransactionStatus =
      sync(async(n).transactionsStatus(Seq(txId))).head

    def transactionsByAddress(address: String, limit: Int): Seq[TransactionInfo] =
      sync(async(n).transactionsByAddress(address, limit))

    def transactionsByAddress(address: String, limit: Int, after: String): Seq[TransactionInfo] =
      sync(async(n).transactionsByAddress(address, limit, after))

    def scriptCompile(code: String): CompiledScript =
      sync(async(n).scriptCompile(code))

    def scriptDecompile(code: String): DecompiledScript =
      sync(async(n).scriptDecompile(code))

    def scriptEstimate(code: String): EstimatedScript =
      sync(async(n).scriptEstimate(code))

    def getAddresses: Seq[String] = sync(async(n).getAddresses)

    def burn(
        sender: KeyPair,
        assetId: String,
        quantity: Long,
        fee: Long = burnFee,
        version: TxVersion = TxVersion.V1,
        waitForTx: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).burn(sender, assetId, quantity, fee, version)), waitForTx)

    def sponsorAsset(
        sender: KeyPair,
        assetId: String,
        baseFee: Long,
        fee: Long = sponsorReducedFee,
        version: Byte = 1,
        waitForTx: Boolean = false,
        amountsAsStrings: Boolean = false
    ): Transaction = {
      maybeWaitForTransaction(sync(async(n).sponsorAsset(sender, assetId, Some(baseFee), fee, version, amountsAsStrings)), waitForTx)
    }

    def cancelSponsorship(sender: KeyPair, assetId: String, fee: Long = sponsorFee, version: Byte = 1): Transaction =
      sync(async(n).cancelSponsorship(sender, assetId, fee, version))

    def createAlias(
        sender: KeyPair,
        alias: String,
        fee: Long = minFee,
        version: TxVersion = TxVersion.V2,
        waitForTx: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).createAlias(sender, alias, fee, version)), waitForTx)

    def aliasByAddress(targetAddress: String): Seq[String] =
      sync(async(n).aliasByAddress(targetAddress))

    def broadcastTransfer(
        source: KeyPair,
        recipient: String,
        amount: Long,
        fee: Long = minFee,
        assetId: Option[String] = None,
        feeAssetId: Option[String] = None,
        attachment: ByteStr = ByteStr.empty,
        version: Byte = TxVersion.V2,
        waitForTx: Boolean = false
    ): Transaction = {
      val tx = TransferTransaction
        .selfSigned(
          version = version,
          sender = source,
          recipient = AddressOrAlias.fromString(recipient).explicitGet(),
          asset = Asset.fromString(assetId),
          amount = amount,
          feeAsset = Asset.fromString(feeAssetId),
          fee = fee,
          attachment = attachment,
          timestamp = System.currentTimeMillis()
        )
        .explicitGet()

      maybeWaitForTransaction(sync(async(n).broadcastRequest(tx.json())), wait = waitForTx)
    }

    def broadcastExchange(
        matcher: KeyPair,
        order1: Order,
        order2: Order,
        amount: TxExchangeAmount,
        price: TxExchangePrice,
        buyMatcherFee: Long,
        sellMatcherFee: Long,
        fee: Long,
        version: Byte = 2,
        waitForTx: Boolean = false,
        amountsAsStrings: Boolean = false,
        validate: Boolean = true
    ): Transaction = {
      maybeWaitForTransaction(
        sync(
          async(n).broadcastExchange(
            matcher,
            order1,
            order2,
            amount,
            price,
            buyMatcherFee,
            sellMatcherFee,
            fee,
            version,
            amountsAsStrings,
            validate
          )
        ),
        waitForTx
      )
    }

    def transfer(
        sender: KeyPair,
        recipient: String,
        amount: Long,
        fee: Long = minFee,
        assetId: Option[String] = None,
        feeAssetId: Option[String] = None,
        version: Byte = TxVersion.V2,
        attachment: Option[String] = None,
        waitForTx: Boolean = false
    ): Transaction = {
      maybeWaitForTransaction(
        sync(async(n).transfer(sender, recipient, amount, fee, assetId, feeAssetId, version, attachment)),
        waitForTx
      )
    }

    def massTransfer(
        sender: KeyPair,
        transfers: List[Transfer],
        fee: Long,
        version: TxVersion = TxVersion.V2,
        attachment: Option[String] = None,
        assetId: Option[String] = None,
        waitForTx: Boolean = false,
        amountsAsStrings: Boolean = false
    ): Transaction = {
      maybeWaitForTransaction(
        sync(async(n).massTransfer(sender, transfers, fee, version, attachment, assetId, amountsAsStrings)),
        waitForTx
      )
    }

    def broadcastLease(
        source: KeyPair,
        recipient: String,
        leasingAmount: Long,
        leasingFee: Long = minFee,
        waitForTx: Boolean = false
    ): Transaction = {
      val tx = LeaseTransaction
        .selfSigned(
          2.toByte,
          sender = source,
          recipient = AddressOrAlias.fromString(recipient).explicitGet(),
          amount = leasingAmount,
          fee = leasingFee,
          timestamp = System.currentTimeMillis()
        )
        .explicitGet()

      maybeWaitForTransaction(sync(async(n).broadcastRequest(tx.json())), wait = waitForTx)
    }

    def lease(
        sender: KeyPair,
        recipient: String,
        leasingAmount: Long,
        leasingFee: Long = minFee,
        version: TxVersion = TxVersion.V1,
        waitForTx: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).lease(sender, recipient, leasingAmount, leasingFee, version)), waitForTx)

    def putData(
        sender: KeyPair,
        data: List[DataEntry[?]],
        fee: Long,
        waitForTx: Boolean = false,
        version: TxVersion = 1.toByte,
        amountsAsStrings: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).broadcastData(sender, data, fee, version = version, amountsAsStrings = amountsAsStrings)), waitForTx)

    def broadcastData(
        sender: KeyPair,
        data: List[DataEntry[?]],
        fee: Long,
        version: TxVersion = TxVersion.V2,
        timestamp: Option[Long] = None,
        waitForTx: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).broadcastData(sender, data, fee, version, timestamp)), waitForTx)

    def removeData(sender: KeyPair, data: Seq[String], fee: Long, version: Byte = 2): Transaction =
      sync(async(n).removeData(sender, data, fee, version))

    def getData(sourceAddress: String, amountsAsStrings: Boolean = false): List[DataEntry[?]] =
      sync(async(n).getData(sourceAddress, amountsAsStrings))

    def getData(sourceAddress: String, regexp: String): List[DataEntry[?]] =
      sync(async(n).getData(sourceAddress, regexp))

    def getDataByKey(sourceAddress: String, key: String): DataEntry[?] =
      sync(async(n).getDataByKey(sourceAddress, key))

    def getDataList(sourceAddress: String, keys: String*): Seq[DataEntry[?]] =
      sync(async(n).getDataList(sourceAddress, keys*))

    def getDataListJson(sourceAddress: String, keys: String*): Seq[DataEntry[?]] =
      sync(async(n).getDataListJson(sourceAddress, keys*))

    def getDataListPost(sourceAddress: String, keys: String*): Seq[DataEntry[?]] =
      sync(async(n).getDataListPost(sourceAddress, keys*))

    def getMerkleProof(ids: String*): Seq[MerkleProofResponse] = sync(async(n).getMerkleProof(ids*))

    def getMerkleProofPost(ids: String*): Seq[MerkleProofResponse] = sync(async(n).getMerkleProofPost(ids*))

    def broadcastRequest[A: Writes](req: A): Transaction =
      sync(async(n).broadcastRequest(req))

    // since activation of SynchronousCalls
    def activeLeases(sourceAddress: String): Seq[LeaseInfo] =
      sync(async(n).activeLeases(sourceAddress))

    def broadcastCancelLease(source: KeyPair, leaseId: String, fee: Long = minFee, waitForTx: Boolean = false): Transaction = {
      val tx = LeaseCancelTransaction
        .selfSigned(
          TxVersion.V2,
          source,
          ByteStr.decodeBase58(leaseId).get,
          fee,
          System.currentTimeMillis()
        )
        .explicitGet()

      maybeWaitForTransaction(sync(async(n).broadcastRequest(tx.json())), wait = waitForTx)
    }

    def cancelLease(
        sender: KeyPair,
        leaseId: String,
        fee: Long = minFee,
        version: TxVersion = TxVersion.V1,
        waitForTx: Boolean = false
    ): Transaction =
      maybeWaitForTransaction(sync(async(n).cancelLease(sender, leaseId, fee, version)), waitForTx)

    def expectSignedBroadcastRejected(json: JsValue): Int = sync(async(n).expectSignedBroadcastRejected(json))

    def signedBroadcast(tx: JsValue, waitForTx: Boolean = false): Transaction = {
      maybeWaitForTransaction(sync(async(n).signedBroadcast(tx)), waitForTx)
    }

    def signedIssue(tx: IssueRequest): Transaction =
      sync(async(n).signedIssue(tx))

    def ensureTxDoesntExist(txId: String): Unit =
      sync(async(n).ensureTxDoesntExist(txId))

    def createKeyPair(): SeedKeyPair = sync(async(n).createKeyPair())

    def createKeyPairServerSide(): KeyPair = sync(async(n).createKeyPairServerSide())

    def rawTransactionInfo(txId: String): JsValue =
      sync(async(n).rawTransactionInfo(txId))

    def waitForTransaction(txId: String, timeout: FiniteDuration = 2.minutes): TransactionInfo =
      sync(async(n).waitForTransaction(txId), timeout)

    def waitForHeight(expectedHeight: Int, requestAwaitTime: FiniteDuration = RequestAwaitTime): Int =
      sync(async(n).waitForHeight(expectedHeight), requestAwaitTime)

    def blacklist(address: InetSocketAddress): Unit =
      sync(async(n).blacklist(address))

    def clearBlacklist(): Unit =
      sync(async(n).clearBlacklist())

    def debugMinerInfo(): Seq[State] =
      sync(async(n).debugMinerInfo())

    def transactionSerializer(body: JsObject): TransactionSerialize = sync(async(n).transactionSerializer(body))

    def debugStateAt(height: Long): Map[String, Long] = sync(async(n).debugStateAt(height))

    def debugBalanceHistory(address: String, amountsAsStrings: Boolean = false): Seq[BalanceHistory] =
      sync(async(n).debugBalanceHistory(address, amountsAsStrings))

    def height: Int =
      sync(async(n).height)

    def blockAt(height: Int, amountsAsStrings: Boolean = false): Block = sync(async(n).blockAt(height, amountsAsStrings))

    def blockSeq(fromHeight: Int, toHeight: Int, amountsAsStrings: Boolean = false): Seq[Block] =
      sync(async(n).blockSeq(fromHeight, toHeight, amountsAsStrings))

    def blockSeqByAddress(address: String, from: Int, to: Int, amountsAsStrings: Boolean = false): Seq[Block] =
      sync(async(n).blockSeqByAddress(address, from, to, amountsAsStrings))

    def blockHeadersSeq(fromHeight: Int, toHeight: Int, amountsAsStrings: Boolean = false): Seq[BlockHeader] =
      sync(async(n).blockHeadersSeq(fromHeight, toHeight, amountsAsStrings))

    def rollback(to: Int, returnToUTX: Boolean = true): Unit =
      sync(async(n).rollback(to, returnToUTX))

    def findTransactionInfo(txId: String): Option[TransactionInfo] = sync(async(n).findTransactionInfo(txId))

    def connectedPeers: Seq[Peer] = (Json.parse(get("/peers/connected").getResponseBody) \ "peers").as[Seq[Peer]]

    def calculateFee(tx: JsObject, amountsAsStrings: Boolean = false): FeeInfo =
      sync(async(n).calculateFee(tx, amountsAsStrings))

    def blacklistedPeers: Seq[BlacklistedPeer] =
      sync(async(n).blacklistedPeers)

    def waitFor[A](desc: String)(f: Node => A, cond: A => Boolean, retryInterval: FiniteDuration): A =
      sync(async(n).waitFor[A](desc)(x => Future.successful(f(x.n)), cond, retryInterval), 5.minutes)

    def waitForEmptyUtx(): Unit =
      sync(async(n).waitFor("empty utx")(_.utxSize, (_: Int) == 0, 1 second), 5 minutes)

    def waitForBlackList(blackList: Int): Seq[BlacklistedPeer] =
      sync(async(n).waitForBlackList(blackList))

    def status(): Status =
      sync(async(n).status)

    def waitForPeers(targetPeersCount: Int, requestAwaitTime: FiniteDuration = RequestAwaitTime): Seq[Peer] =
      sync(async(n).waitForPeers(targetPeersCount), requestAwaitTime)

    def connect(address: InetSocketAddress): Unit =
      sync(async(n).connect(address))

    def setScript(
        sender: KeyPair,
        script: Option[String] = None,
        fee: Long = setScriptFee,
        version: Byte = TxVersion.V1,
        waitForTx: Boolean = false
    ): Transaction = {
      maybeWaitForTransaction(sync(async(n).setScript(sender, script, fee, version)), waitForTx)
    }

    def setAssetScript(
        assetId: String,
        sender: KeyPair,
        fee: Long = issueFee,
        script: Option[String] = None,
        version: Byte = 1,
        waitForTx: Boolean = false
    ): Transaction = {
      maybeWaitForTransaction(sync(async(n).setAssetScript(assetId, sender, fee, script, version)), waitForTx)
    }

    def invokeScript(
        caller: KeyPair,
        dappAddress: String,
        func: Option[String] = None,
        args: List[Terms.EXPR] = List.empty,
        payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty,
        fee: Long = smartMinFee,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V1,
        waitForTx: Boolean = false
    ): (Transaction, JsValue) = {
      sync(async(n).invokeScript(caller, dappAddress, func, args, payment, fee, feeAssetId, version)) match {
        case (tx, js) => maybeWaitForTransaction(tx, waitForTx) -> js
      }
    }

    def invokeExpression(
        caller: KeyPair,
        expression: ExprScript,
        fee: Long = invokeExpressionFee,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V1,
        waitForTx: Boolean = false
    ): (Transaction, JsValue) =
      sync(async(n).invokeExpression(caller, expression, fee, feeAssetId, version)) match {
        case (tx, js) => maybeWaitForTransaction(tx, waitForTx) -> js
      }

    def validateInvokeScript(
        caller: KeyPair,
        dappAddress: String,
        func: Option[String] = None,
        args: List[Terms.EXPR] = List.empty,
        payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty,
        fee: Long = smartMinFee,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V1
    ): (JsValue, JsValue) = {
      sync(async(n).validateInvokeScript(caller, dappAddress, func, args, payment, fee, feeAssetId, version))
    }

    def updateAssetInfo(
        caller: KeyPair,
        assetId: String,
        name: String,
        description: String,
        fee: Long = issueFee,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V1,
        timestamp: Option[Long] = None,
        waitForTx: Boolean = false
    ): (Transaction, JsValue) = {
      sync(async(n).updateAssetInfo(caller, assetId, name, description, fee, feeAssetId, version, timestamp)) match {
        case (tx, js) => maybeWaitForTransaction(tx, waitForTx) -> js
      }
    }

    def waitForUtxIncreased(fromSize: Int): Int = sync(async(n).waitForUtxIncreased(fromSize))

    def featureActivationStatus(featureNum: Short): FeatureActivationStatus =
      activationStatus.features.find(_.id == featureNum).get

    def signedValidate(json: JsValue): JsValue = sync(async(n).signedValidate(json))
  }

  implicit class NodesExtSync(nodes: Seq[Node]) {

    import com.wavesplatform.it.api.AsyncHttpApi.NodesAsyncHttpApi as async

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

    def waitFor[A](desc: String, retryInterval: FiniteDuration = 1.second)(request: Node => A)(cond: Iterable[A] => Boolean): Boolean =
      sync(
        async(nodes).waitFor(desc)(retryInterval)((n: Node) => Future(request(n))(scala.concurrent.ExecutionContext.Implicits.global), cond),
        ConditionAwaitTime
      )

    def waitForEmptyUtx(): Unit =
      waitFor("empty utx")(_.utxSize)(_.forall(_ == 0))

    def rollbackWithoutBlacklisting(height: Int, returnToUTX: Boolean = true): Unit = {
      sync(
        Future.traverse(nodes) { node =>
          com.wavesplatform.it.api.AsyncHttpApi.NodeAsyncHttpApi(node).rollback(height, returnToUTX)
        },
        ConditionAwaitTime
      )
    }

    def rollback(height: Int, returnToUTX: Boolean = true): Unit = {
      val combinations = nodes.combinations(2).toSeq
      combinations.foreach { ns =>
        ns.head.blacklist(ns(1).networkAddress)
        ns(1).blacklist(ns.head.networkAddress)
      }

      nodes.rollbackWithoutBlacklisting(height, returnToUTX)
      nodes.foreach(_.clearBlacklist())

      combinations.foreach { ns =>
        ns.head.connect(ns(1).networkAddress)
      }
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
