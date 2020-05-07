package com.wavesplatform.it.api

import java.io.IOException
import java.net.{InetSocketAddress, URLEncoder}
import java.util.concurrent.TimeoutException
import java.util.{NoSuchElementException, UUID}

import com.wavesplatform.account.{AddressScheme, KeyPair}
import com.wavesplatform.api.http.ConnectReq
import com.wavesplatform.api.http.RewardApiRoute.RewardStatus
import com.wavesplatform.api.http.requests.{IssueRequest, TransferRequest}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.api.ActivationStatus
import com.wavesplatform.http.DebugMessage._
import com.wavesplatform.http.{DebugMessage, RollbackParams, `X-Api-Key`}
import com.wavesplatform.it.Node
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, DataEntry, EmptyDataEntry, Portfolio}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{Order, ExchangeTransaction => ExchangeTx}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction, Proofs, TxVersion}
import org.asynchttpclient.Dsl.{delete => _delete, get => _get, post => _post, put => _put}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants.ResponseStatusCodes.OK_200
import org.scalactic.source.Position
import org.scalatest.{Assertions, Matchers}
import play.api.libs.json.Json.{stringify, toJson}
import play.api.libs.json._

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object AsyncHttpApi extends Assertions {

  //noinspection ScalaStyle
  implicit class NodeAsyncHttpApi(val n: Node) extends Assertions with Matchers {

    def get(
        path: String,
        amountsAsStrings: Boolean = false,
        withApiKey: Boolean = false,
        f: RequestBuilder => RequestBuilder = identity
    ): Future[Response] = {
      val defaultReqBuilder = _get(s"${n.nodeApiEndpoint}$path")

      if (amountsAsStrings || withApiKey) {
        if (amountsAsStrings && withApiKey) {
          retrying(f(defaultReqBuilder.setHeader("Accept", "application/json;large-significand-format=string").withApiKey(n.apiKey)).build())
        } else {
          if (withApiKey) {
            retrying(f(defaultReqBuilder.withApiKey(n.apiKey)).build())
          } else {
            retrying(f(defaultReqBuilder.setHeader("Accept", "application/json;large-significand-format=string")).build())
          }
        }
      } else {
        retrying(f(defaultReqBuilder).build())
      }
    }

    def getWithCustomHeader(
        path: String,
        headerName: String = "Accept",
        headerValue: String,
        withApiKey: Boolean = false,
        f: RequestBuilder => RequestBuilder = identity
    ): Future[Response] = {
      val requestBuilder = if (withApiKey) {
        _get(s"${n.nodeApiEndpoint}$path").setHeader(headerName, headerValue).withApiKey(n.apiKey)
      } else {
        _get(s"${n.nodeApiEndpoint}$path").setHeader(headerName, headerValue)
      }
      retrying(f(requestBuilder).build())
    }

    def delete(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(_delete(s"${n.nodeApiEndpoint}$path")).withApiKey(n.apiKey).build())

    def seed(address: String): Future[String] = getWithApiKey(s"/addresses/seed/$address").as[JsValue].map(v => (v \ "seed").as[String])

    def getWithApiKey(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] = retrying {
      _get(s"${n.nodeApiEndpoint}$path")
        .withApiKey(n.apiKey)
        .build()
    }

    def postJsonWithApiKey[A: Writes](path: String, body: A): Future[Response] = retrying {
      _post(s"${n.nodeApiEndpoint}$path")
        .withApiKey(n.apiKey)
        .setHeader("Content-type", "application/json;charset=utf-8")
        .setBody(stringify(toJson(body)))
        .build()
    }

    def postJsObjectWithApiKey(path: String, body: JsValue): Future[Response] = retrying {
      _post(s"${n.nodeApiEndpoint}$path")
        .withApiKey(n.apiKey)
        .setHeader("Content-type", "application/json")
        .setBody(stringify(body))
        .build()
    }

    def postJsObjectWithCustomHeader(path: String, body: JsValue, headerName: String = "Accept", headerValue: String): Future[Response] = retrying {
      _post(s"${n.nodeApiEndpoint}$path")
        .withApiKey(n.apiKey)
        .setHeader("Content-type", "application/json")
        .setHeader(headerName, headerValue)
        .setBody(stringify(body))
        .build()
    }

    def post(url: String, f: RequestBuilder => RequestBuilder = identity, waitForStatus: Boolean = false): Future[Response] =
      retrying(f(_post(url).withApiKey(n.apiKey)).build(), waitForStatus = waitForStatus)

    def put(url: String, f: RequestBuilder => RequestBuilder = identity, statusCode: Int = OK_200, waitForStatus: Boolean = false): Future[Response] =
      retrying(f(_put(url).withApiKey(n.apiKey)).build(), waitForStatus = waitForStatus, statusCode = statusCode)

    def postJson[A: Writes](path: String, body: A): Future[Response] =
      post(path, stringify(toJson(body)))

    def post(path: String, body: String): Future[Response] =
      post(s"${n.nodeApiEndpoint}$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json;charset=utf-8").setBody(body))

    def postForm(path: String, params: (String, String)*): Future[Response] =
      post(
        s"${n.nodeApiEndpoint}$path",
        (rb: RequestBuilder) =>
          rb.setHeader("Content-type", "application/x-www-form-urlencoded").setBody(params.map(p => p._1 + "=" + p._2).mkString("&"))
      )

    def blacklist(address: InetSocketAddress): Future[Unit] =
      post("/debug/blacklist", s"${address.getHostString}:${address.getPort}").map(_ => ())

    def printDebugMessage(db: DebugMessage): Future[Response] = postJsonWithApiKey("/debug/print", db)

    def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
      (Json.parse(r.getResponseBody) \ "peers").as[Seq[Peer]]
    }

    def blacklistedPeers: Future[Seq[BlacklistedPeer]] = get("/peers/blacklisted").map { r =>
      Json.parse(r.getResponseBody).as[Seq[BlacklistedPeer]]
    }

    def connect(address: InetSocketAddress): Future[Unit] =
      postJson("/peers/connect", ConnectReq(address.getHostName, address.getPort)).map(_ => ())

    def waitForStartup(): Future[Option[Response]] = {
      val timeout = 500

      def request =
        _get(s"${n.nodeApiEndpoint}/blocks/height?${System.currentTimeMillis()}")
          .setReadTimeout(timeout)
          .setRequestTimeout(timeout)
          .build()

      def send(): Future[Option[Response]] =
        n.client
          .executeRequest(request)
          .toCompletableFuture
          .toScala
          .map(Option(_))
          .recoverWith {
            case _: IOException | _: TimeoutException => Future(None)
          }

      def cond(ropt: Option[Response]) = ropt.exists { r =>
        r.getStatusCode == OK_200 && (Json.parse(r.getResponseBody) \ "height").as[Int] > 0
      }

      waitFor("node is up")(_ => send(), cond, 1.second)
    }

    def waitForPeers(targetPeersCount: Int): Future[Seq[Peer]] =
      waitFor[Seq[Peer]](s"connectedPeers.size >= $targetPeersCount")(_.connectedPeers, _.lengthCompare(targetPeersCount) >= 0, 1.second)

    def waitForBlackList(blackListSize: Int): Future[Seq[BlacklistedPeer]] =
      waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers > $blackListSize")(_.blacklistedPeers, _.lengthCompare(blackListSize) > 0, 500.millis)

    def height: Future[Int] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Int])

    def blockAt(height: Int, amountsAsStrings: Boolean = false): Future[Block] =
      get(s"/blocks/at/$height", amountsAsStrings).as[Block](amountsAsStrings)

    def blockById(id: String, amountsAsStrings: Boolean = false): Future[Block] = get(s"/blocks/$id", amountsAsStrings).as[Block](amountsAsStrings)

    def utx(amountsAsStrings: Boolean = false): Future[Seq[Transaction]] = {
      get(s"/transactions/unconfirmed", amountsAsStrings).as[Seq[Transaction]](amountsAsStrings)
    }

    def utxById(txId: String, amountsAsStrings: Boolean = false): Future[Transaction] = {
      get(s"/transactions/unconfirmed/info/$txId", amountsAsStrings).as[Transaction](amountsAsStrings)
    }

    def utxSize: Future[Int] = get(s"/transactions/unconfirmed/size").as[JsObject].map(_.value("size").as[Int])

    def lastBlock(amountsAsStrings: Boolean = false): Future[Block] = get("/blocks/last", amountsAsStrings).as[Block](amountsAsStrings)

    def blockSeq(from: Int, to: Int, amountsAsStrings: Boolean = false): Future[Seq[Block]] =
      get(s"/blocks/seq/$from/$to", amountsAsStrings)
        .as[Seq[Block]](amountsAsStrings)

    def blockSeqByAddress(address: String, from: Int, to: Int, amountsAsStrings: Boolean = false): Future[Seq[Block]] =
      get(s"/blocks/address/$address/$from/$to", amountsAsStrings)
        .as[Seq[Block]](amountsAsStrings)

    def blockHeadersAt(height: Int, amountsAsStrings: Boolean = false): Future[BlockHeader] =
      get(s"/blocks/headers/at/$height", amountsAsStrings)
        .as[BlockHeader](amountsAsStrings)

    def blockHeaderForId(id: String, amountsAsStrings: Boolean = false): Future[BlockHeader] =
      get(s"/blocks/headers/$id", amountsAsStrings)
        .as[BlockHeader](amountsAsStrings)

    def blockHeadersSeq(from: Int, to: Int, amountsAsStrings: Boolean = false): Future[Seq[BlockHeader]] =
      get(s"/blocks/headers/seq/$from/$to", amountsAsStrings)
        .as[Seq[BlockHeader]](amountsAsStrings)

    def lastBlockHeader(amountsAsStrings: Boolean = false): Future[BlockHeader] =
      get("/blocks/headers/last", amountsAsStrings)
        .as[BlockHeader](amountsAsStrings)

    def status: Future[Status] = get("/node/status").as[Status]

    def blockGenerationSignature(signature: String): Future[GenerationSignatureResponse] =
      get(s"/consensus/generationsignature/$signature").as[GenerationSignatureResponse]

    def lastBlockGenerationSignature: Future[String] = get(s"/consensus/generationsignature").as[String]

    def generatingBalance(address: String, amountsAsStrings: Boolean = false): Future[GeneratingBalance] = {
      get(s"/consensus/generatingbalance/$address", amountsAsStrings).as[GeneratingBalance](amountsAsStrings)
    }

    def activationStatus: Future[ActivationStatus] = get("/activation/status").as[ActivationStatus]

    def rewardStatus(height: Option[Int] = None, amountsAsString: Boolean = false): Future[RewardStatus] = {
      val maybeHeight = height.fold("")(a => s"/$a")
      get(s"/blockchain/rewards$maybeHeight", amountsAsString).as[RewardStatus](amountsAsString)
    }

    def balance(address: String, confirmations: Option[Int] = None, amountsAsStrings: Boolean = false): Future[Balance] = {
      val maybeConfirmations = confirmations.fold("")(a => s"/$a")
      get(s"/addresses/balance/$address$maybeConfirmations", amountsAsStrings).as[Balance](amountsAsStrings)
    }

    def balances(height: Option[Int], addresses: Seq[String], asset: Option[String]): Future[Seq[Balance]] = {
      for {
        json <- postJson(
          "/addresses/balance",
          Json.obj("addresses" -> addresses) ++
            height.fold(Json.obj())(h => Json.obj("height" -> h)) ++
            asset.fold(Json.obj())(a => Json.obj("asset"   -> a))
        )
      } yield Json.parse(json.getResponseBody).as[Seq[JsObject]].map(r => Balance((r \ "id").as[String], 0, (r \ "balance").as[Long]))
    }

    def balanceDetails(address: String, amountsAsStrings: Boolean = false): Future[BalanceDetails] =
      get(s"/addresses/balance/details/$address", amountsAsStrings).as[BalanceDetails](amountsAsStrings)

    def getAddresses: Future[Seq[String]] = get(s"/addresses").as[Seq[String]]

    def scriptInfo(address: String): Future[AddressScriptInfo] =
      get(s"/addresses/scriptInfo/$address").as[AddressScriptInfo]

    def findTransactionInfo(txId: String): Future[Option[TransactionInfo]] = transactionInfo[TransactionInfo](txId).transform {
      case Success(tx)                                          => Success(Some(tx))
      case Failure(UnexpectedStatusCodeException(_, _, 404, _)) => Success(None)
      case Failure(ex)                                          => Failure(ex)
    }

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): Future[TransactionInfo] = {
      val condition = waitFor[Option[TransactionInfo]](s"transaction $txId")(
        _.transactionInfo[TransactionInfo](txId).transform {
          case Success(tx)                                          => Success(Some(tx))
          case Failure(UnexpectedStatusCodeException(_, _, 404, _)) => Success(None)
          case Failure(ex)                                          => Failure(ex)
        },
        tOpt => tOpt.exists(_.id == txId),
        retryInterval
      ).map(_.get)

      condition
    }

    def waitForUtxIncreased(fromSize: Int): Future[Int] = waitFor[Int](s"utxSize > $fromSize")(
      _.utxSize,
      _ > fromSize,
      100.millis
    )

    def waitForHeight(expectedHeight: Int): Future[Int] = waitFor[Int](s"height >= $expectedHeight")(_.height, h => h >= expectedHeight, 2.seconds)

    def rawTransactionInfo(txId: String): Future[JsValue] = get(s"/transactions/info/$txId").map(r => Json.parse(r.getResponseBody))

    def transactionInfo[A: Reads](txId: String, amountsAsStrings: Boolean = false): Future[A] = {
      get(s"/transactions/info/$txId", amountsAsStrings).as[A](amountsAsStrings)
    }

    def transactionsStatus(txIds: Seq[String]): Future[Seq[TransactionStatus]] =
      postJson(s"/transactions/status", Json.obj("ids" -> txIds)).as[List[TransactionStatus]]

    def transactionsByAddress(address: String, limit: Int): Future[Seq[TransactionInfo]] =
      get(s"/transactions/address/$address/limit/$limit").as[Seq[Seq[TransactionInfo]]].map(_.flatten)

    def transactionsByAddress(address: String, limit: Int, after: String): Future[Seq[TransactionInfo]] = {
      get(s"/transactions/address/$address/limit/$limit?after=$after").as[Seq[Seq[TransactionInfo]]].map(_.flatten)
    }

    def assetDistributionAtHeight(
        asset: String,
        height: Int,
        limit: Int,
        maybeAfter: Option[String] = None,
        amountsAsStrings: Boolean = false
    ): Future[AssetDistributionPage] = {
      val after = maybeAfter.fold("")(a => s"?after=$a")
      val url   = s"/assets/$asset/distribution/$height/limit/$limit$after"

      get(url, amountsAsStrings).as[AssetDistributionPage](amountsAsStrings)
    }

    def assetDistribution(asset: String, amountsAsStrings: Boolean = false): Future[AssetDistribution] = {
      val req = s"/assets/$asset/distribution"
      get(req, amountsAsStrings).as[AssetDistribution](amountsAsStrings)
    }

    def effectiveBalance(address: String, confirmations: Option[Int] = None, amountsAsStrings: Boolean = false): Future[Balance] = {
      val maybeConfirmations = confirmations.fold("")(a => s"/$a")
      get(s"/addresses/effectiveBalance/$address$maybeConfirmations", amountsAsStrings).as[Balance](amountsAsStrings)
    }

    def transfer(
        sourceAddress: String,
        recipient: String,
        amount: Long,
        fee: Long,
        assetId: Option[String] = None,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V2,
        typedAttachment: Option[Attachment] = None,
        attachment: Option[String] = None
    ): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"      -> TransferTransaction.typeId,
          "sender"    -> sourceAddress,
          "amount"    -> amount,
          "recipient" -> recipient,
          "fee"       -> fee,
          "version"   -> version,
          "assetId" -> {
            if (assetId.isDefined) JsString(assetId.get) else JsNull
          },
          "feeAssetId" -> {
            if (feeAssetId.isDefined) JsString(feeAssetId.get) else JsNull
          },
          "attachment" -> {
            if (attachment.isDefined || typedAttachment.isDefined) {
              if (attachment.isDefined) {
                attachment.get
              } else Json.toJson(typedAttachment.get)
            } else JsNull
          }
        )
      )
    }

    def payment(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
      postJson("/waves/payment", PaymentRequest(amount, fee, sourceAddress, recipient)).as[Transaction]

    def lease(sourceAddress: String, recipient: String, amount: Long, fee: Long, version: TxVersion = TxVersion.V2): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"      -> LeaseTransaction.typeId,
          "sender"    -> sourceAddress,
          "amount"    -> amount,
          "recipient" -> recipient,
          "fee"       -> fee,
          "version"   -> version
        )
      )
    }

    def cancelLease(sourceAddress: String, leaseId: String, fee: Long, version: TxVersion = TxVersion.V2): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"    -> LeaseCancelTransaction.typeId,
          "sender"  -> sourceAddress,
          "txId"    -> leaseId,
          "fee"     -> fee,
          "version" -> version
        )
      )
    }

    def activeLeases(sourceAddress: String): Future[Seq[Transaction]] = get(s"/leasing/active/$sourceAddress").as[Seq[Transaction]]

    def issue(
        sourceAddress: String,
        name: String,
        description: String,
        quantity: Long,
        decimals: Byte,
        reissuable: Boolean,
        fee: Long,
        version: TxVersion = TxVersion.V2,
        script: Option[String] = None
    ): Future[Transaction] = {
      val js = Json.obj(
        "type"        -> IssueTransaction.typeId,
        "name"        -> name,
        "quantity"    -> quantity,
        "description" -> description,
        "sender"      -> sourceAddress,
        "decimals"    -> decimals,
        "reissuable"  -> reissuable,
        "fee"         -> fee,
        "version"     -> version
      )

      val jsUpdated = if (script.isDefined) js ++ Json.obj("script" -> JsString(script.get)) else js
      signAndBroadcast(jsUpdated)

    }

    def setScript(sender: String, script: Option[String] = None, fee: Long = 1000000, version: TxVersion = TxVersion.V1): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"    -> SetScriptTransaction.typeId,
          "version" -> version,
          "sender"  -> sender,
          "fee"     -> fee,
          "script"  -> { if (script.isDefined) JsString(script.get) else JsNull }
        )
      )
    }

    def setAssetScript(
        assetId: String,
        sender: String,
        fee: Long,
        script: Option[String] = None,
        version: TxVersion = TxVersion.V1
    ): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"    -> SetAssetScriptTransaction.typeId,
          "version" -> version,
          "assetId" -> assetId,
          "sender"  -> sender,
          "fee"     -> fee,
          "script"  -> { if (script.isDefined) JsString(script.get) else JsNull }
        )
      )
    }

    def invokeScript(
        caller: String,
        dappAddress: String,
        func: Option[String],
        args: List[Terms.EXPR] = List.empty,
        payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty,
        fee: Long = 500000,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V1
    ): Future[(Transaction, JsValue)] = {
      signAndTraceBroadcast(
        Json.obj(
          "type"    -> InvokeScriptTransaction.typeId,
          "version" -> version,
          "sender"  -> caller,
          "dApp"    -> dappAddress,
          "call" -> {
            if (func.isDefined) InvokeScriptTransaction.serializer.functionCallToJson(FUNCTION_CALL(FunctionHeader.User(func.get), args)) else JsNull
          },
          "payment"    -> payment,
          "fee"        -> fee,
          "feeAssetId" -> { if (feeAssetId.isDefined) JsString(feeAssetId.get) else JsNull }
        )
      )
    }

    def validateInvokeScript(
        caller: String,
        dappAddress: String,
        func: Option[String],
        args: List[Terms.EXPR] = List.empty,
        payment: Seq[InvokeScriptTransaction.Payment] = Seq.empty,
        fee: Long = 500000,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V1
    ): Future[(JsValue, JsValue)] = {
      signAndValidate(
        Json.obj(
          "type"    -> InvokeScriptTransaction.typeId,
          "version" -> version,
          "sender"  -> caller,
          "dApp"    -> dappAddress,
          "call" -> {
            if (func.isDefined) InvokeScriptTransaction.serializer.functionCallToJson(FUNCTION_CALL(FunctionHeader.User(func.get), args)) else JsNull
          },
          "payment"    -> payment,
          "fee"        -> fee,
          "feeAssetId" -> { if (feeAssetId.isDefined) JsString(feeAssetId.get) else JsNull }
        )
      )
    }

    def updateAssetInfo(
        sender: KeyPair,
        assetId: String,
        name: String,
        description: String,
        fee: Long,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V1,
        timestamp: Option[Long] = None
    ): Future[(Transaction, JsValue)] = {
      val tx = UpdateAssetInfoTransaction(
        version,
        sender.publicKey,
        IssuedAsset(ByteStr(Base58.decode(assetId))),
        name,
        description,
        timestamp.getOrElse(System.currentTimeMillis()),
        fee,
        if (feeAssetId.isDefined) IssuedAsset(ByteStr(Base58.decode(feeAssetId.get))) else Waves,
        Proofs.empty,
        AddressScheme.current.chainId
      ).signWith(sender.privateKey)
      signedTraceBroadcast(tx.json())
    }

    def scriptCompile(code: String): Future[CompiledScript] = post("/utils/script/compileCode", code).as[CompiledScript]

    def scriptDecompile(script: String): Future[DecompiledScript] = post("/utils/script/decompile", script).as[DecompiledScript]

    def scriptEstimate(script: String): Future[EstimatedScript] = post("/utils/script/estimate", script).as[EstimatedScript]

    def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long, version: Byte = 1): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"       -> ReissueTransaction.typeId,
          "sender"     -> sourceAddress,
          "assetId"    -> assetId,
          "quantity"   -> quantity,
          "reissuable" -> reissuable,
          "fee"        -> fee,
          "version"    -> version
        )
      )
    }

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long, version: TxVersion = TxVersion.V2): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"     -> BurnTransaction.typeId,
          "quantity" -> quantity,
          "assetId"  -> assetId,
          "sender"   -> sourceAddress,
          "fee"      -> fee,
          "version"  -> version
        )
      )
    }

    def debugStateChanges(invokeScriptTransactionId: String, amountsAsStrings: Boolean): Future[DebugStateChanges] =
      get(s"/debug/stateChanges/info/$invokeScriptTransactionId", amountsAsStrings).as[DebugStateChanges](amountsAsStrings)

    def debugStateChangesByAddress(address: String, limit: Int = 10000, after: Option[String] = None): Future[Seq[DebugStateChanges]] =
      get(s"/debug/stateChanges/address/$address/limit/$limit${after.fold("")(a => s"?after=$a")}").as[Seq[DebugStateChanges]]

    def assetBalance(address: String, asset: String, amountsAsStrings: Boolean = false): Future[AssetBalance] =
      get(s"/assets/balance/$address/$asset", amountsAsStrings).as[AssetBalance](amountsAsStrings)

    def assetsBalance(address: String, amountsAsStrings: Boolean = false): Future[FullAssetsInfo] =
      get(s"/assets/balance/$address", amountsAsStrings).as[FullAssetsInfo](amountsAsStrings)

    def nftList(address: String, limit: Int, maybeAfter: Option[String] = None, amountsAsStrings: Boolean = false): Future[Seq[NFTAssetInfo]] = {
      val after = maybeAfter.fold("")(a => s"?after=$a")
      get(s"/assets/nft/$address/limit/$limit$after", amountsAsStrings).as[Seq[NFTAssetInfo]](amountsAsStrings)
    }

    def assetsDetails(assetId: String, fullInfo: Boolean = false, amountsAsStrings: Boolean = false): Future[AssetInfo] = {
      get(s"/assets/details/$assetId?full=$fullInfo", amountsAsStrings).as[AssetInfo](amountsAsStrings)
    }

    def sponsorAsset(
        sourceAddress: String,
        assetId: String,
        minSponsoredAssetFee: Long,
        fee: Long,
        version: Byte = 1,
        amountsAsStrings: Boolean = false
    ): Future[Transaction] =
      signAndBroadcast(
        Json.obj(
          "type"                 -> SponsorFeeTransaction.typeId,
          "assetId"              -> assetId,
          "sender"               -> sourceAddress,
          "fee"                  -> fee,
          "version"              -> version,
          "minSponsoredAssetFee" -> minSponsoredAssetFee
        ),
        amountsAsStrings
      )

    def cancelSponsorship(sourceAddress: String, assetId: String, fee: Long, version: Byte = 1): Future[Transaction] =
      signAndBroadcast(
        Json.obj(
          "type"                 -> SponsorFeeTransaction.typeId,
          "assetId"              -> assetId,
          "sender"               -> sourceAddress,
          "fee"                  -> fee,
          "version"              -> version,
          "minSponsoredAssetFee" -> JsNull
        )
      )

    def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
      postJson(
        "/assets/transfer",
        TransferRequest(Some(1.toByte), Some(sourceAddress), None, recipient, None, amount, None, fee, None, None, None, None)
      ).as[Transaction]

    def massTransfer(
        sourceAddress: String,
        transfers: List[Transfer],
        fee: Long,
        version: TxVersion = TxVersion.V2,
        typedAttachment: Option[Attachment] = None,
        attachment: Option[String] = None,
        assetId: Option[String] = None,
        amountsAsStrings: Boolean = false
    ): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"      -> MassTransferTransaction.typeId,
          "assetId"   -> { if (assetId.isDefined) JsString(assetId.get) else JsNull },
          "sender"    -> sourceAddress,
          "fee"       -> fee,
          "version"   -> version,
          "transfers" -> Json.toJson(transfers),
          "attachment" -> {
            if (attachment.isDefined || typedAttachment.isDefined) {
              if (attachment.isDefined) {
                attachment.get
              } else Json.toJson(typedAttachment.get)
            } else JsNull
          }
        ),
        amountsAsStrings
      )
    }

    def putData(
        sourceAddress: String,
        data: List[DataEntry[_]],
        fee: Long,
        version: TxVersion = 1.toByte,
        amountsAsStrings: Boolean = false
    ): Future[Transaction] =
      signAndBroadcast(
        Json.obj("type" -> DataTransaction.typeId, "sender" -> sourceAddress, "fee" -> fee, "version" -> version, "data" -> data),
        amountsAsStrings
      )

    def broadcastData(
        sender: KeyPair,
        data: List[DataEntry[_]],
        fee: Long,
        version: TxVersion = TxVersion.V2,
        timestamp: Option[Long] = None
    ): Future[Transaction] = {
      val tx = DataTransaction(
        version,
        sender.publicKey,
        data,
        fee,
        timestamp.getOrElse(System.currentTimeMillis()),
        Proofs.empty,
        AddressScheme.current.chainId
      ).signWith(sender.privateKey)
      signedBroadcast(tx.json())
    }

    def removeData(sourceAddress: String, data: Seq[String], fee: Long, version: Byte = 2): Future[Transaction] = {
      signAndBroadcast(
        Json
          .obj("type" -> DataTransaction.typeId, "sender" -> sourceAddress, "fee" -> fee, "version" -> version, "data" -> data.map(EmptyDataEntry(_)))
      )
    }

    def getData(address: String, amountsAsStrings: Boolean = false): Future[List[DataEntry[_]]] =
      get(s"/addresses/data/$address", amountsAsStrings).as[List[DataEntry[_]]](amountsAsStrings)

    def getData(address: String, regexp: String): Future[List[DataEntry[_]]] = get(s"/addresses/data/$address?matches=$regexp").as[List[DataEntry[_]]]

    def getDataByKey(address: String, key: String): Future[DataEntry[_]] = get(s"/addresses/data/$address/$key").as[DataEntry[_]]

    def getDataListJson(address: String, keys: String*): Future[Seq[DataEntry[_]]] =
      postJson(s"/addresses/data/$address", Json.obj("keys" -> keys)).as[Seq[DataEntry[_]]]

    def getDataListPost(address: String, keys: String*): Future[Seq[DataEntry[_]]] =
      postForm(s"/addresses/data/$address", keys.map("key" -> URLEncoder.encode(_, "UTF-8")): _*).as[Seq[DataEntry[_]]]

    def getDataList(address: String, keys: String*): Future[Seq[DataEntry[_]]] =
      get(s"/addresses/data/$address?${keys.map("key=" + URLEncoder.encode(_, "UTF-8")).mkString("&")}").as[Seq[DataEntry[_]]]

    def getMerkleProof(ids: String*): Future[Seq[MerkleProofResponse]] =
      get(s"/transactions/merkleProof?${ids.map("id=" + URLEncoder.encode(_, "UTF-8")).mkString("&")}").as[Seq[MerkleProofResponse]]

    def getMerkleProofPost(ids: String*): Future[Seq[MerkleProofResponse]] =
      postJson(s"/transactions/merkleProof", Json.obj("ids" -> ids)).as[Seq[MerkleProofResponse]]

    def broadcastRequest[A: Writes](req: A): Future[Transaction] = postJson("/transactions/broadcast", req).as[Transaction]

    def broadcastTraceRequest[A: Writes](req: A): Future[Transaction] = postJson("/transactions/broadcast?trace=yes", req).as[Transaction]

    def sign(json: JsValue, amountsAsStrings: Boolean = false): Future[JsObject] =
      postJsObjectWithApiKey("/transactions/sign", json).as[JsObject]

    def expectSignedBroadcastRejected(json: JsValue): Future[Int] = {
      post("/transactions/broadcast", stringify(json)).transform {
        case Failure(UnexpectedStatusCodeException(_, _, 400, body)) => Success((Json.parse(body) \ "error").as[Int])
        case Failure(cause)                                          => Failure(cause)
        case Success(resp)                                           => Failure(UnexpectedStatusCodeException("POST", "/transactions/broadcast", resp.getStatusCode, resp.getResponseBody))
      }
    }

    def signedBroadcast(json: JsValue, amountsAsStrings: Boolean = false): Future[Transaction] = {
      if (amountsAsStrings) {
        postJsObjectWithCustomHeader("/transactions/broadcast", json, headerValue = "application/json;large-significand-format=string")
          .as[Transaction](amountsAsStrings)
      } else {
        post("/transactions/broadcast", stringify(json)).as[Transaction]
      }
    }

    def signedBroadcast(json: JsValue): Future[Transaction] =
      post("/transactions/broadcast", stringify(json)).as[Transaction]

    def signedTraceBroadcast(json: JsValue): Future[(Transaction, JsValue)] =
      post("/transactions/broadcast?trace=yes", stringify(json)).as[JsValue].map(r => (r.as[Transaction], r))

    def signedValidate(json: JsValue): Future[JsValue] = post("/debug/validate", stringify(json)).as[JsValue]

    def signAndBroadcast(json: JsValue, amountsAsStrings: Boolean = false): Future[Transaction] =
      sign(json, amountsAsStrings).flatMap(signedBroadcast(_, amountsAsStrings))

    def signAndTraceBroadcast(json: JsValue): Future[(Transaction, JsValue)] = sign(json).flatMap(signedTraceBroadcast)

    def signAndValidate(json: JsValue): Future[(JsValue, JsValue)] = sign(json).flatMap(signed => signedValidate(signed).map((signed, _)))

    def signedIssue(issue: IssueRequest): Future[Transaction] =
      signedBroadcast(issue.toTx.explicitGet().json())

    def batchSignedTransfer(transfers: Seq[TransferRequest], timeout: FiniteDuration = 1.minute): Future[Seq[Transaction]] = {
      import TransferRequest.jsonFormat
      Future.sequence(transfers.map(v => signedBroadcast(toJson(v).as[JsObject] ++ Json.obj("type" -> TransferTransaction.typeId.toInt))))
    }

    def createAlias(targetAddress: String, alias: String, fee: Long, version: TxVersion = TxVersion.V2): Future[Transaction] =
      signAndBroadcast(
        Json.obj(
          "type"    -> CreateAliasTransaction.typeId,
          "version" -> version,
          "sender"  -> targetAddress,
          "fee"     -> fee,
          "alias"   -> alias
        )
      )

    def broadcastExchange(
        matcher: KeyPair,
        order1: Order,
        order2: Order,
        amount: Long,
        price: Long,
        buyMatcherFee: Long,
        sellMatcherFee: Long,
        fee: Long,
        version: Byte,
        matcherFeeAssetId: Option[String],
        amountsAsStrings: Boolean = false,
        validate: Boolean = true
    ): Future[Transaction] = {
      val tx = ExchangeTx(
        version = version,
        order1 = order1,
        order2 = order2,
        amount = amount,
        price = price,
        buyMatcherFee = buyMatcherFee,
        sellMatcherFee = sellMatcherFee,
        fee = fee,
        proofs = Proofs.empty,
        timestamp = System.currentTimeMillis(),
        chainId = AddressScheme.current.chainId
      ).signWith(matcher.privateKey)

      val json = if (validate) tx.validatedEither.right.get.json() else tx.json()
      signedBroadcast(json, amountsAsStrings)
    }

    def aliasByAddress(targetAddress: String): Future[Seq[String]] =
      get(s"/alias/by-address/$targetAddress").as[Seq[String]]

    def addressByAlias(targetAlias: String): Future[Address] =
      get(s"/alias/by-alias/$targetAlias").as[Address]

    def rollback(to: Int, returnToUTX: Boolean = true): Future[Unit] =
      postJson("/debug/rollback", RollbackParams(to, returnToUTX)).map(_ => ())

    def rollbackToBlockId(id: String): Future[Unit] = delete(s"/debug/rollback-to/$id").map(_ => ())

    def ensureTxDoesntExist(txId: String): Future[Unit] =
      utx()
        .zip(findTransactionInfo(txId))
        .flatMap({
          case (utx, _) if utx.map(_.id).contains(txId) =>
            Future.failed(new IllegalStateException(s"Tx $txId is in UTX"))
          case (_, txOpt) if txOpt.isDefined =>
            Future.failed(new IllegalStateException(s"Tx $txId is in blockchain"))
          case _ =>
            Future.successful(())
        })

    def waitFor[A](desc: String)(f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] = {
      n.log.debug(s"Awaiting condition '$desc'")
      timer
        .retryUntil(f(this), cond, retryInterval)
        .map(a => {
          n.log.debug(s"Condition '$desc' met")
          a
        })
    }

    def createAddress: Future[String] =
      post(s"${n.nodeApiEndpoint}/addresses").as[JsValue].map(v => (v \ "address").as[String])

    def waitForNextBlock: Future[BlockHeader] =
      for {
        currentBlock <- lastBlockHeader()
        actualBlock  <- findBlockHeaders(_.height > currentBlock.height, currentBlock.height)
      } yield actualBlock

    def waitForHeightArise: Future[Int] =
      for {
        height    <- height
        newHeight <- waitForHeight(height + 1)
      } yield newHeight

    def findBlock(cond: Block => Boolean, from: Int = 1, to: Int = Int.MaxValue): Future[Block] = {
      def load(_from: Int, _to: Int): Future[Block] = blockSeq(_from, _to).flatMap { blocks =>
        blocks
          .find(cond)
          .fold[Future[Block]] {
            val maybeLastBlock = blocks.lastOption
            if (maybeLastBlock.exists(_.height >= to)) {
              Future.failed(new NoSuchElementException)
            } else {
              val newFrom = maybeLastBlock.fold(_from)(b => (b.height + 19).min(to))
              val newTo   = newFrom + 19
              n.log.debug(s"Loaded ${blocks.length} blocks, no match found. Next range: [$newFrom, ${newFrom + 19}]")
              timer.schedule(load(newFrom, newTo), n.settings.blockchainSettings.genesisSettings.averageBlockDelay)
            }
          }(Future.successful)
      }

      load(from, (from + 19).min(to))
    }

    def findBlockHeaders(cond: BlockHeader => Boolean, from: Int = 1, to: Int = Int.MaxValue): Future[BlockHeader] = {
      def load(_from: Int, _to: Int): Future[BlockHeader] = blockHeadersSeq(_from, _to).flatMap { blocks =>
        blocks
          .find(cond)
          .fold[Future[BlockHeader]] {
            val maybeLastBlock = blocks.lastOption
            if (maybeLastBlock.exists(_.height >= to)) {
              Future.failed(new NoSuchElementException)
            } else {
              val newFrom = maybeLastBlock.fold(_from)(b => (b.height + 19).min(to))
              val newTo   = newFrom + 19
              n.log.debug(s"Loaded ${blocks.length} blocks, no match found. Next range: [$newFrom, ${newFrom + 19}]")
              timer.schedule(load(newFrom, newTo), n.settings.blockchainSettings.genesisSettings.averageBlockDelay)
            }
          }(Future.successful)
      }

      load(from, (from + 19).min(to))
    }

    def getGeneratedBlocks(address: String, from: Long, to: Long): Future[Seq[Block]] =
      get(s"/blocks/address/$address/$from/$to").as[Seq[Block]]

    def retrying(r: Request, interval: FiniteDuration = 1.second, statusCode: Int = OK_200, waitForStatus: Boolean = false): Future[Response] = {
      def executeRequest: Future[Response] = {
        val id = UUID.randomUUID()
        n.log.trace(s"[$id] Executing request '$r'")
        if (r.getStringData != null) n.log.debug(s"[$id] Request's body '${r.getStringData}'")
        n.client
          .executeRequest(
            r,
            new AsyncCompletionHandler[Response] {
              override def onCompleted(response: Response): Response = {
                if (response.getStatusCode == statusCode) {
                  n.log.debug(s"[$id] Request: ${r.getMethod} ${r.getUrl}\nResponse: ${response.getResponseBody}")
                  response
                } else {
                  n.log.debug(
                    s"[$id] Request: ${r.getMethod} ${r.getUrl}\nUnexpected status code(${response.getStatusCode}): ${response.getResponseBody}"
                  )
                  throw UnexpectedStatusCodeException(r.getMethod, r.getUrl, response.getStatusCode, response.getResponseBody)
                }
              }
            }
          )
          .toCompletableFuture
          .toScala
          .recoverWith {
            case e: UnexpectedStatusCodeException if e.statusCode == 503 || waitForStatus =>
              n.log.debug(s"[$id] Failed to execute request '$r' with error: ${e.getMessage}")
              timer.schedule(executeRequest, interval)
            case e @ (_: IOException | _: TimeoutException) =>
              n.log.debug(s"[$id] Failed to execute request '$r' with error: ${e.getMessage}")
              timer.schedule(executeRequest, interval)
          }
      }

      executeRequest
    }

    def once(r: Request): Future[Response] = {
      val id = UUID.randomUUID()
      n.log.trace(s"[$id] Executing request ${r.getMethod} ${r.getUrl}")
      n.client
        .executeRequest(
          r,
          new AsyncCompletionHandler[Response] {
            override def onCompleted(response: Response): Response = {
              n.log.debug(s"[$id] Response for ${r.getUrl} is ${response.getStatusCode}")
              response
            }
          }
        )
        .toCompletableFuture
        .toScala
    }

    def debugStateAt(height: Long): Future[Map[String, Long]] = getWithApiKey(s"/debug/stateWaves/$height").as[Map[String, Long]]

    def debugBalanceHistory(address: String, amountsAsStrings: Boolean = false): Future[Seq[BalanceHistory]] = {
      get(s"/debug/balances/history/$address", withApiKey = true, amountsAsStrings = amountsAsStrings)
        .as[Seq[BalanceHistory]](amountsAsStrings)
    }

    def debugPortfoliosFor(address: String, considerUnspent: Boolean, amountsAsStrings: Boolean = false): Future[Portfolio] = {
      get(s"/debug/portfolios/$address?considerUnspent=$considerUnspent", withApiKey = true, amountsAsStrings = amountsAsStrings)
        .as[Portfolio](amountsAsStrings)
    }

    def debugMinerInfo(): Future[Seq[State]] = getWithApiKey(s"/debug/minerInfo").as[Seq[State]]

    def transactionSerializer(body: JsObject): Future[TransactionSerialize] =
      postJsObjectWithApiKey(s"/utils/transactionSerialize", body).as[TransactionSerialize]

    def accountEffectiveBalance(acc: String): Future[Long] = n.effectiveBalance(acc).map(_.balance)

    def accountBalance(acc: String): Future[Long] = n.balance(acc).map(_.balance)

    def accountsBalances(height: Option[Int], accounts: Seq[String], asset: Option[String]): Future[Seq[(String, Long)]] = {
      n.balances(height, accounts, asset).map(_.map(b => (b.address, b.balance)))
    }

    def accountBalances(acc: String): Future[(Long, Long)] = {
      n.balance(acc).map(_.balance).zip(n.effectiveBalance(acc).map(_.balance))
    }

    def assertBalances(acc: String, balance: Long, effectiveBalance: Long)(implicit pos: Position): Future[Unit] = {
      for {
        newBalance          <- accountBalance(acc)
        newEffectiveBalance <- accountEffectiveBalance(acc)
      } yield {
        withClue(s"effective balance of $acc") {
          newEffectiveBalance shouldBe effectiveBalance
        }
        withClue(s"balance of $acc") {
          newBalance shouldBe balance
        }
      }
    }

    def assertAssetBalance(acc: String, assetIdString: String, balance: Long)(implicit pos: Position): Future[Unit] = {
      for {
        plainBalance <- n.assetBalance(acc, assetIdString)
        pf           <- n.assetsBalance(acc)
        asset        <- n.assetsDetails(assetIdString)
        nftList      <- n.nftList(acc, 100)
      } yield {
        plainBalance.balance shouldBe balance
        if (asset.isNFT) {
          nftList.count(_.assetId == assetIdString) shouldBe balance
        } else if (balance != 0) {
          pf.balances.find(_.assetId == assetIdString).map(_.balance) should contain(balance)
        }
      }
    }

    def calculateFee(json: JsValue, amountsAsStrings: Boolean = false): Future[FeeInfo] = {
      if (amountsAsStrings) {
        postJsObjectWithCustomHeader("/transactions/calculateFee", json, headerValue = "application/json;large-significand-format=string")
          .as[FeeInfo](amountsAsStrings)
      } else {
        postJsObjectWithApiKey("/transactions/calculateFee", json).as[FeeInfo]
      }
    }
  }

  implicit class NodesAsyncHttpApi(nodes: Seq[Node]) extends Matchers {
    def height: Future[Seq[Int]] = traverse(nodes)(_.height)

    def waitForHeightAriseAndTxPresent(transactionId: String)(implicit p: Position): Future[Unit] =
      for {
        allHeights   <- traverse(nodes)(_.waitForTransaction(transactionId).map(_.height))
        _            <- traverse(nodes)(_.waitForHeight(allHeights.max + 1))
        finalHeights <- traverse(nodes)(_.waitForTransaction(transactionId).map(_.height))
        _ <- waitFor("nodes sync")(1 second)(
          _.waitForTransaction(transactionId).map(_.height),
          (finalHeights: Iterable[Int]) => finalHeights.forall(_ == finalHeights.head)
        )
      } yield ()

    def waitForTransaction(transactionId: String)(implicit p: Position): Future[TransactionInfo] =
      traverse(nodes)(_.waitForTransaction(transactionId)).map(_.head)

    def waitForHeightArise(): Future[Int] =
      for {
        height <- height.map(_.max)
        _      <- traverse(nodes)(_.waitForHeight(height + 1))
      } yield height + 1

    def waitForSameBlockHeadersAt(height: Int, retryInterval: FiniteDuration = 5.seconds): Future[Boolean] = {

      def waitHeight = waitFor[Int](s"all heights >= $height")(retryInterval)(_.height, _.forall(_ >= height))

      def waitSameBlockHeaders =
        waitFor[BlockHeader](s"same blocks at height = $height")(retryInterval)(_.blockHeadersAt(height), { blocks =>
          val id = blocks.map(_.id)
          id.forall(_ == id.head)
        })

      for {
        _ <- waitHeight
        r <- waitSameBlockHeaders
      } yield r
    }

    def waitFor[A](desc: String)(retryInterval: FiniteDuration)(request: Node => Future[A], cond: Iterable[A] => Boolean): Future[Boolean] = {
      def retry = timer.schedule(waitFor(desc)(retryInterval)(request, cond), retryInterval)

      Future
        .traverse(nodes)(request)
        .map(cond)
        .recover { case _ => false }
        .flatMap {
          case true  => Future.successful(true)
          case false => retry
        }
    }

  }

  implicit class RequestBuilderOps(self: RequestBuilder) {
    def withApiKey(x: String): RequestBuilder = self.setHeader(`X-Api-Key`.name, x)
  }
}
