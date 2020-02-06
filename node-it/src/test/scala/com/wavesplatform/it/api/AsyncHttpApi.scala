package com.wavesplatform.it.api

import java.io.IOException
import java.net.{InetSocketAddress, URLEncoder}
import java.util.concurrent.TimeoutException
import java.util.{NoSuchElementException, UUID}

import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.{AddressScheme, Alias, KeyPair}
import com.wavesplatform.api.grpc.BalanceResponse.WavesBalances
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.api.grpc._
import com.wavesplatform.api.http.RewardApiRoute.RewardStatus
import com.wavesplatform.api.http.requests.{IssueRequest, TransferRequest}
import com.wavesplatform.api.http.{AddressApiRoute, ConnectReq}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.features.api.ActivationStatus
import com.wavesplatform.http.DebugMessage._
import com.wavesplatform.http.{DebugMessage, RollbackParams, `X-Api-Key`}
import com.wavesplatform.it.Node
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import com.wavesplatform.lang.script.{Script, ScriptReader}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.protobuf.transaction.{Attachment => PBAttachment, Recipient => PBRecipient, Script => _, _}
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, DataEntry, EmptyDataEntry, Portfolio}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.{BurnTransaction, IssueTransaction, SetAssetScriptTransaction, SponsorFeeTransaction, UpdateAssetInfoTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.serialization.impl.BaseTxJson
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, DataTransaction, Proofs, TxVersion}
import io.grpc.stub.StreamObserver
import monix.eval.Task
import monix.reactive.subjects.ConcurrentSubject
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

    def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(_get(s"${n.nodeApiEndpoint}$path")).build())

    def getWithCustomHeader(path: String, headerName: String, headerValue: String, withApiKey: Boolean = false, f: RequestBuilder => RequestBuilder = identity): Future[Response] = {
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

    def postJsObjectWithCustomHeader(path: String, body: JsValue, headerName: String, headerValue: String): Future[Response] = retrying {
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

    def blockAt(height: Int): Future[Block] = get(s"/blocks/at/$height").as[Block]

    def blockBySignature(signature: String): Future[Block] = get(s"/blocks/signature/$signature").as[Block]

    def utx: Future[Seq[Transaction]] = get(s"/transactions/unconfirmed").as[Seq[Transaction]]

    def utxSize: Future[Int] = get(s"/transactions/unconfirmed/size").as[JsObject].map(_.value("size").as[Int])

    def lastBlock: Future[Block] = get("/blocks/last").as[Block]

    def blockSeq(from: Int, to: Int): Future[Seq[Block]] = get(s"/blocks/seq/$from/$to").as[Seq[Block]]

    def blockSeqByAddress(address: String, from: Int, to: Int): Future[Seq[Block]] = get(s"/blocks/address/$address/$from/$to").as[Seq[Block]]

    def blockHeadersAt(height: Int): Future[BlockHeaders] = get(s"/blocks/headers/at/$height").as[BlockHeaders]

    def blockHeadersSeq(from: Int, to: Int): Future[Seq[BlockHeaders]] = get(s"/blocks/headers/seq/$from/$to").as[Seq[BlockHeaders]]

    def lastBlockHeaders: Future[BlockHeaders] = get("/blocks/headers/last").as[BlockHeaders]

    def status: Future[Status] = get("/node/status").as[Status]

    def blockGenerationSignature(signature: String): Future[GenerationSignatureResponse] =
      get(s"/consensus/generationsignature/$signature").as[GenerationSignatureResponse]

    def lastBlockGenerationSignature: Future[String] = get(s"/consensus/generationsignature").as[String]

    def activationStatus: Future[ActivationStatus] = get("/activation/status").as[ActivationStatus]

    def rewardStatus(height: Int): Future[RewardStatus] = get(s"/blockchain/rewards/$height").as[RewardStatus]

    def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]

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

    def balanceDetails(address: String): Future[BalanceDetails] = get(s"/addresses/balance/details/$address").as[BalanceDetails]

    def getAddresses: Future[Seq[String]] = get(s"/addresses").as[Seq[String]]

    def scriptInfo(address: String): Future[AddressApiRoute.AddressScriptInfo] =
      get(s"/addresses/scriptInfo/$address").as[AddressApiRoute.AddressScriptInfo]

    def findTransactionInfo(txId: String): Future[Option[TransactionInfo]] = transactionInfo(txId).transform {
      case Success(tx)                                          => Success(Some(tx))
      case Failure(UnexpectedStatusCodeException(_, _, 404, _)) => Success(None)
      case Failure(ex)                                          => Failure(ex)
    }

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): Future[TransactionInfo] = {
      val condition = waitFor[Option[TransactionInfo]](s"transaction $txId")(
        _.transactionInfo(txId).transform {
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

    def waitForHeight(expectedHeight: Int): Future[Int] = waitFor[Int](s"height >= $expectedHeight")(_.height, h => h >= expectedHeight, 5.seconds)

    def rawTransactionInfo(txId: String): Future[JsValue] = get(s"/transactions/info/$txId").map(r => Json.parse(r.getResponseBody))

    def transactionInfo(txId: String): Future[TransactionInfo] = get(s"/transactions/info/$txId").as[TransactionInfo]

    def transactionsByAddress(address: String, limit: Int): Future[Seq[TransactionInfo]] =
      get(s"/transactions/address/$address/limit/$limit").as[Seq[Seq[TransactionInfo]]].map(_.flatten)

    def transactionsByAddress(address: String, limit: Int, after: String): Future[Seq[TransactionInfo]] = {
      get(s"/transactions/address/$address/limit/$limit?after=$after").as[Seq[Seq[TransactionInfo]]].map(_.flatten)
    }

    def assetDistributionAtHeight(asset: String, height: Int, limit: Int, maybeAfter: Option[String] = None): Future[AssetDistributionPage] = {
      val after = maybeAfter.fold("")(a => s"?after=$a")
      val url   = s"/assets/$asset/distribution/$height/limit/$limit$after"

      get(url).as[AssetDistributionPage]
    }

    def assetDistribution(asset: String): Future[AssetDistribution] = {
      val req = s"/assets/$asset/distribution"
      get(req).as[AssetDistribution]
    }

    def effectiveBalance(address: String): Future[Balance] = get(s"/addresses/effectiveBalance/$address").as[Balance]

    def transfer(
        sourceAddress: String,
        recipient: String,
        amount: Long,
        fee: Long,
        assetId: Option[String] = None,
        feeAssetId: Option[String] = None,
        version: TxVersion = TxVersion.V2
    ): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"       -> TransferTransaction.typeId,
          "sender"     -> sourceAddress,
          "amount"     -> amount,
          "recipient"  -> recipient,
          "fee"        -> fee,
          "version"    -> version,
          "assetId"    -> { if (assetId.isDefined) JsString(assetId.get) else JsNull },
          "feeAssetId" -> { if (feeAssetId.isDefined) JsString(feeAssetId.get) else JsNull }
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
        AddressScheme.current.chainId,
        sender.publicKey,
        IssuedAsset(ByteStr(Base58.decode(assetId))),
        name,
        description,
        timestamp.getOrElse(System.currentTimeMillis()),
        fee,
        if (feeAssetId.isDefined) IssuedAsset(ByteStr(Base58.decode(feeAssetId.get))) else Waves,
        Proofs.empty
      ).signWith(sender.privateKey)
      signedTraceBroadcast(tx.json())
    }

    def scriptCompile(code: String): Future[CompiledScript] = post("/utils/script/compileCode", code).as[CompiledScript]

    def scriptDecompile(script: String): Future[DecompiledScript] = post("/utils/script/decompile", script).as[DecompiledScript]

    def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Future[Transaction] =
      postJson(
        "/assets/reissue",
        Json.obj(
          "sender"     -> sourceAddress,
          "assetId"    -> assetId,
          "quantity"   -> quantity,
          "reissuable" -> reissuable,
          "fee"        -> fee
        )
      ).as[Transaction]

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

    def debugStateChanges(invokeScriptTransactionId: String): Future[DebugStateChanges] =
      get(s"/debug/stateChanges/info/$invokeScriptTransactionId").as[DebugStateChanges]

    def debugStateChangesByAddress(address: String, limit: Int = 10000): Future[Seq[DebugStateChanges]] =
      get(s"/debug/stateChanges/address/$address/limit/$limit").as[Seq[DebugStateChanges]]

    def assetBalance(address: String, asset: String): Future[AssetBalance] =
      get(s"/assets/balance/$address/$asset").as[AssetBalance]

    def assetsBalance(address: String): Future[FullAssetsInfo] =
      get(s"/assets/balance/$address").as[FullAssetsInfo]

    def nftAssetsBalance(address: String, limit: Int): Future[Seq[NFTAssetInfo]] =
      get(s"/assets/nft/$address/limit/$limit").as[Seq[NFTAssetInfo]]

    def nftAssetsBalance(address: String, limit: Int, after: String): Future[Seq[NFTAssetInfo]] =
      get(s"/assets/nft/$address/limit/${limit}?after=$after").as[Seq[NFTAssetInfo]]

    def assetsDetails(assetId: String, fullInfo: Boolean = false): Future[AssetInfo] =
      get(s"/assets/details/$assetId?full=$fullInfo").as[AssetInfo]

    def sponsorAsset(sourceAddress: String, assetId: String, minSponsoredAssetFee: Long, fee: Long): Future[Transaction] =
      signAndBroadcast(
        Json.obj(
          "type"                 -> SponsorFeeTransaction.typeId,
          "assetId"              -> assetId,
          "sender"               -> sourceAddress,
          "fee"                  -> fee,
          "version"              -> 1,
          "minSponsoredAssetFee" -> minSponsoredAssetFee
        )
      )

    def cancelSponsorship(sourceAddress: String, assetId: String, fee: Long): Future[Transaction] =
      signAndBroadcast(
        Json.obj(
          "type"                 -> SponsorFeeTransaction.typeId,
          "assetId"              -> assetId,
          "sender"               -> sourceAddress,
          "fee"                  -> fee,
          "version"              -> 1,
          "minSponsoredAssetFee" -> JsNull
        )
      )

    def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
      postJson(
        "/assets/transfer",
        TransferRequest(Some(1.toByte), Some(sourceAddress), None, recipient, None, amount, None, fee, None, None, None, None)
      ).as[Transaction]

    def massTransfer(sourceAddress: String, transfers: List[Transfer], fee: Long, assetId: Option[String] = None): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"      -> MassTransferTransaction.typeId,
          "assetId"   -> { if (assetId.isDefined) JsString(assetId.get) else JsNull },
          "sender"    -> sourceAddress,
          "fee"       -> fee,
          "version"   -> 1,
          "transfers" -> Json.toJson(transfers)
        )
      )
    }

    def putData(sourceAddress: String, data: List[DataEntry[_]], fee: Long): Future[Transaction] = {
      signAndBroadcast(Json.obj("type" -> DataTransaction.typeId, "sender" -> sourceAddress, "fee" -> fee, "version" -> 1, "data" -> data))
    }

    def removeData(sourceAddress: String, data: Seq[String], fee: Long): Future[Transaction] = {
      signAndBroadcast(
        Json.obj("type" -> DataTransaction.typeId, "sender" -> sourceAddress, "fee" -> fee, "version" -> 2, "data" -> data.map(EmptyDataEntry(_)))
      )
    }


    def getData(address: String): Future[List[DataEntry[_]]] = get(s"/addresses/data/$address").as[List[DataEntry[_]]]

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

    def sign(json: JsValue): Future[JsObject] =
      postJsObjectWithApiKey("/transactions/sign", json).as[JsObject]

    def expectSignedBroadcastRejected(json: JsValue): Future[Int] = {
      post("/transactions/broadcast", stringify(json)).transform {
        case Failure(UnexpectedStatusCodeException(_, _, 400, body)) => Success((Json.parse(body) \ "error").as[Int])
        case Failure(cause)                                          => Failure(cause)
        case Success(resp)                                           => Failure(UnexpectedStatusCodeException("POST", "/transactions/broadcast", resp.getStatusCode, resp.getResponseBody))
      }
    }

    def signedBroadcast(json: JsValue): Future[Transaction] =
      post("/transactions/broadcast", stringify(json)).as[Transaction]

    def signedTraceBroadcast(json: JsValue): Future[(Transaction, JsValue)] =
      post("/transactions/broadcast?trace=yes", stringify(json)).as[JsValue].map(r => (r.as[Transaction], r))

    def signAndBroadcast(json: JsValue): Future[Transaction] = sign(json).flatMap(signedBroadcast)

    def signAndTraceBroadcast(json: JsValue): Future[(Transaction, JsValue)] = sign(json).flatMap(signedTraceBroadcast)

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

    def broadcastExchange(matcher: KeyPair,
                 buyOrder: Order,
                 sellOrder: Order,
                 amount: Long,
                 price: Long,
                 buyMatcherFee: Long,
                 sellMatcherFee: Long,
                 fee: Long,
                 version: Byte,
                 matcherFeeAssetId: Option[String]): Future[Transaction] = {
      val tx = ExchangeTx
        .signed(
          matcher = matcher,
          buyOrder = buyOrder,
          sellOrder = sellOrder,
          amount = amount,
          price = price,
          buyMatcherFee = buyMatcherFee,
          sellMatcherFee = sellMatcherFee,
          fee = fee,
          timestamp = System.currentTimeMillis(),
          version = version
        )
        .right
        .get
        .json()

      signedBroadcast(tx)
    }

    def aliasByAddress(targetAddress: String): Future[Seq[String]] =
      get(s"/alias/by-address/$targetAddress").as[Seq[String]]

    def addressByAlias(targetAlias: String): Future[Address] =
      get(s"/alias/by-alias/$targetAlias").as[Address]

    def rollback(to: Int, returnToUTX: Boolean = true): Future[Unit] =
      postJson("/debug/rollback", RollbackParams(to, returnToUTX)).map(_ => ())

    def rollbackToBlockWithSignature(signature: String): Future[Unit] = delete(s"/debug/rollback-to/$signature").map(_ => ())

    def ensureTxDoesntExist(txId: String): Future[Unit] =
      utx
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

    def waitForNextBlock: Future[BlockHeaders] =
      for {
        currentBlock <- lastBlockHeaders
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

    def findBlockHeaders(cond: BlockHeaders => Boolean, from: Int = 1, to: Int = Int.MaxValue): Future[BlockHeaders] = {
      def load(_from: Int, _to: Int): Future[BlockHeaders] = blockHeadersSeq(_from, _to).flatMap { blocks =>
        blocks
          .find(cond)
          .fold[Future[BlockHeaders]] {
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

    def debugPortfoliosFor(address: String, considerUnspent: Boolean): Future[Portfolio] = {
      getWithApiKey(s"/debug/portfolios/$address?considerUnspent=$considerUnspent")
    }.as[Portfolio]

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
      n.assetBalance(acc, assetIdString).map(_.balance shouldBe balance)
    }

    def calculateFee(json: JsValue): Future[FeeInfo] =
      postJsObjectWithApiKey("/transactions/calculateFee", json).as[FeeInfo]

    def grpc: NodeExtGrpc = new NodeExtGrpc(n)
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
        waitFor[BlockHeaders](s"same blocks at height = $height")(retryInterval)(_.blockHeadersAt(height), { blocks =>
          val sig = blocks.map(_.signature)
          sig.forall(_ == sig.head)
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
  class NodeExtGrpc(n: Node) {

    import com.wavesplatform.protobuf.transaction.{Script => PBScript, Transaction => PBTransaction}
    import monix.execution.Scheduler.Implicits.global

    private[this] lazy val accounts     = AccountsApiGrpc.stub(n.grpcChannel)
    private[this] lazy val blocks       = BlocksApiGrpc.stub(n.grpcChannel)
    private[this] lazy val transactions = TransactionsApiGrpc.stub(n.grpcChannel)
    private[this] lazy val assets = AssetsApiGrpc.stub(n.grpcChannel)

    val chainId: Byte = AddressScheme.current.chainId

    def blockAt(height: Int): Future[Block] = {
      blocks
        .getBlock(BlockRequest.of(includeTransactions = true, BlockRequest.Request.Height(height)))
        .map(r => PBBlocks.vanilla(r.getBlock).get.json().as[Block])
    }

    def broadcastIssue(
        source: KeyPair,
        name: String,
        quantity: Long,
        decimals: Int,
        reissuable: Boolean,
        fee: Long,
        description: String = "",
        script: Option[String] = None,
        version: Int = 2
    ): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(source.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        System.currentTimeMillis(),
        version,
        PBTransaction.Data.Issue(
          IssueTransactionData.of(
            name,
            description,
            quantity,
            decimals,
            reissuable,
            toPBScriptOption(script)
          )
        )
      )
      script match {
        case Some(scr) if ScriptReader.fromBytes(Base64.decode(scr)).isLeft =>
          transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.EMPTY)))
        case _ =>
          val proofs = crypto.sign(source, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
          transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
      }
    }

    private def toPBScriptOption(script: Option[String]) = {
      script.map(s => {
        val bytes = {
          val b64 = if (s.startsWith("base64:")) s.drop(7) else s
          Base64.decode(b64)
        }
        PBScript.of(ByteString.copyFrom(bytes.drop(1)), bytes.headOption.fold(0)(_.toInt))
      })
    }

    def broadcastTransfer(
        source: KeyPair,
        recipient: PBRecipient,
        amount: Long,
        fee: Long,
        version: Int = 2,
        assetId: String = "WAVES",
        feeAssetId: String = "WAVES",
        attachment: ByteString = ByteString.EMPTY,
        timestamp: Long = System.currentTimeMillis
    ): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(source.publicKey),
        Some(Amount.of(if (feeAssetId == "WAVES") ByteString.EMPTY else ByteString.copyFrom(Base58.decode(feeAssetId)), fee)),
        timestamp,
        version,
        PBTransaction.Data.Transfer(
          TransferTransactionData.of(
            Some(recipient),
            Some(Amount.of(if (assetId == "WAVES") ByteString.EMPTY else ByteString.copyFrom(Base58.decode(assetId)), amount)),
            Some(PBAttachment.of(PBAttachment.Attachment.BinaryValue(attachment)))
          )
        )
      )
      val proofs      = crypto.sign(source, PBTransactions.vanilla(SignedTransaction(Some(unsigned))).right.get.bodyBytes())
      val transaction = SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs)))

      transactions.broadcast(transaction)
    }

    def broadcastReissue(
        source: KeyPair,
        fee: Long,
        assetId: String,
        amount: Long,
        reissuable: Boolean = false,
        version: Int = 2
    ): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(source.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        System.currentTimeMillis(),
        version,
        PBTransaction.Data.Reissue(
          ReissueTransactionData.of(
            Some(Amount.of(ByteString.copyFrom(Base58.decode(assetId)), amount)),
            reissuable
          )
        )
      )

      val proofs      = crypto.sign(source, PBTransactions.vanilla(SignedTransaction(Some(unsigned))).explicitGet().bodyBytes())
      val transaction = SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs)))

      transactions.broadcast(transaction)
    }

    def broadcastCreateAlias(source: KeyPair, alias: String, fee: Long, version: Int = 2): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(source.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        System.currentTimeMillis,
        version,
        PBTransaction.Data.CreateAlias(CreateAliasTransactionData(alias))
      )
      if (Alias.create(alias).isLeft) {
        transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.EMPTY)))
      } else {
        val proofs = crypto.sign(source, PBTransactions.vanilla(SignedTransaction(Some(unsigned))).explicitGet().bodyBytes())
        transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
      }
    }

    def putData(
        source: KeyPair,
        data: Seq[DataTransactionData.DataEntry],
        fee: Long,
        version: Int = 1,
        timestamp: Long = System.currentTimeMillis()
    ): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(source.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        timestamp,
        version,
        PBTransaction.Data.DataTransaction(DataTransactionData.of(data))
      )
      if (PBTransactions.vanilla(SignedTransaction(Some(unsigned))).isLeft) {
        transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.EMPTY)))
      } else {
        val proofs = crypto.sign(source, PBTransactions.vanilla(SignedTransaction(Some(unsigned))).explicitGet().bodyBytes())
        transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
      }
    }

    def exchange(
        matcher: KeyPair,
        buyOrder: Order,
        sellOrder: Order,
        amount: Long,
        price: Long,
        buyMatcherFee: Long,
        sellMatcherFee: Long,
        fee: Long,
        timestamp: Long,
        version: Byte,
        matcherFeeAssetId: String = "WAVES"
    ): Future[PBSignedTransaction] = {

      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(matcher.publicKey),
        Some(Amount.of(if (matcherFeeAssetId == "WAVES") ByteString.EMPTY else ByteString.copyFrom(Base58.decode(matcherFeeAssetId)), fee)),
        timestamp,
        version,
        PBTransaction.Data.Exchange(
          ExchangeTransactionData.of(
            amount,
            price,
            buyMatcherFee,
            sellMatcherFee,
            Seq(PBOrders.protobuf(buyOrder), PBOrders.protobuf(sellOrder))
          )
        )
      )

      val proofs      = crypto.sign(matcher, PBTransactions.vanilla(SignedTransaction(Some(unsigned))).right.get.bodyBytes())
      val transaction = SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs)))

      transactions.broadcast(transaction)
    }

    def updateAssetInfo(sender: KeyPair,
                        assetId: String,
                        updatedName: String,
                        updatedDescription: String,
                        fee: Long,
                        feeAsset: Asset = Waves,
                        version: TxVersion = TxVersion.V1): Future[SignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(sender.publicKey),
        Some(Amount.of(if (feeAsset == Waves) ByteString.EMPTY else ByteString.copyFrom(Base58.decode(feeAsset.maybeBase58Repr.get)), fee)),
        System.currentTimeMillis(),
        version,
        PBTransaction.Data.UpdateAssetInfo(
          UpdateAssetInfoTransactionData.of(
            ByteString.copyFrom(Base58.decode(assetId)),
            updatedName,
            updatedDescription
          )
        )
      )

      val proofs      = crypto.sign(sender, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
      val transaction = SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs)))

      transactions.broadcast(transaction)
    }

    def setScript(
        sender: KeyPair,
        script: Option[Script],
        fee: Long,
        timestamp: Long = System.currentTimeMillis(),
        version: Int = 1
    ): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(sender.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        timestamp,
        version,
        PBTransaction.Data.SetScript(
          SetScriptTransactionData.of(script.map(s => PBScript.of(ByteString.copyFrom(s.bytes().drop(1)), s.stdLibVersion.id)))
        )
      )

      val proofs = crypto.sign(sender, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
      transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
    }

    def getTransaction(id: String, sender: ByteString = ByteString.EMPTY, recipient: Option[PBRecipient] = None): Future[PBSignedTransaction] = {
      def createCallObserver[T]: (StreamObserver[T], Task[List[T]]) = {
        val subj = ConcurrentSubject.publishToOne[T]

        val observer = new StreamObserver[T] {
          override def onNext(value: T): Unit      = subj.onNext(value)
          override def onError(t: Throwable): Unit = subj.onError(t)
          override def onCompleted(): Unit         = subj.onComplete()
        }

        (observer, subj.toListL)
      }
      val (obs, result) = createCallObserver[TransactionResponse]
      val req           = TransactionsRequest(transactionIds = Seq(ByteString.copyFrom(Base58.decode(id))), sender = sender, recipient = recipient)
      transactions.getTransactions(req, obs)
      result.map(_.headOption.getOrElse(throw new NoSuchElementException("Transaction not found")).getTransaction).runToFuture
    }

    def waitFor[A](desc: String)(f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration): Future[A] = {
      n.log.debug(s"Awaiting condition '$desc'")
      timer
        .retryUntil(f(this), cond, retryInterval)
        .map(a => {
          n.log.debug(s"Condition '$desc' met")
          a
        })
    }

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): Future[PBSignedTransaction] = {
      val condition = waitFor[Option[PBSignedTransaction]](s"transaction $txId")(
        _.getTransaction(txId)
          .map(Option(_))
          .recover { case _: NoSuchElementException => None },
        tOpt => tOpt.exists(t => PBTransactions.vanilla(t).explicitGet().id().toString == txId),
        retryInterval
      ).map(_.get)

      condition
    }

    def height: Future[Int] = blocks.getCurrentHeight(Empty.of()).map(h => h.value)

    def waitForHeight(expectedHeight: Int): Future[Int] = {
      waitFor[Int](s"height >= $expectedHeight")(_.height, h => h >= expectedHeight, 5.seconds)
    }

    def wavesBalance(address: ByteString): Future[WavesBalances] = {
      def createCallObserver[T]: (StreamObserver[T], Task[List[T]]) = {
        val subj = ConcurrentSubject.publishToOne[T]

        val observer = new StreamObserver[T] {
          override def onNext(value: T): Unit      = subj.onNext(value)
          override def onError(t: Throwable): Unit = subj.onError(t)
          override def onCompleted(): Unit         = subj.onComplete()
        }

        (observer, subj.toListL)
      }
      val (obs, result) = createCallObserver[BalanceResponse]
      val req           = BalancesRequest.of(address, Seq(ByteString.EMPTY))
      accounts.getBalances(req, obs)
      result.map(_.headOption.getOrElse(throw new NoSuchElementException("Balances not found for address")).getWaves).runToFuture
    }

    def assetInfo(assetId: String):Future[AssetInfoResponse] = assets.getInfo(AssetRequest(ByteString.copyFrom(Base58.decode(assetId))))

    def broadcastBurn(source: KeyPair, assetId: String, amount: Long, fee: Long, version: Int = 2): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(source.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        System.currentTimeMillis(),
        version,
        PBTransaction.Data.Burn(
          BurnTransactionData.of(
            Some(Amount.of(ByteString.copyFrom(Base58.decode(assetId)), amount))
          )
        )
      )

      val proofs      = crypto.sign(source, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
      val transaction = SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs)))
      transactions.broadcast(transaction)
    }

    def broadcast(unsignedTx: PBTransaction, proofs: Seq[ByteString]): Future[PBSignedTransaction] =
      transactions.broadcast(SignedTransaction(Some(unsignedTx), proofs))

    def broadcastSponsorFee(sender: KeyPair, minFee: Option[Amount], fee: Long, version: Int = 1): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(sender.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        System.currentTimeMillis,
        version,
        PBTransaction.Data.SponsorFee(SponsorFeeTransactionData.of(minFee))
      )
      val proofs = crypto.sign(sender, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
      transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
    }

    def broadcastMassTransfer(
        sender: KeyPair,
        assetId: Option[String] = None,
        transfers: Seq[MassTransferTransactionData.Transfer],
        attachment: ByteString = ByteString.EMPTY,
        fee: Long,
        version: Int = 1
    ): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(sender.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        System.currentTimeMillis(),
        version,
        PBTransaction.Data.MassTransfer(
          MassTransferTransactionData.of(
            if (assetId.isDefined) ByteString.copyFrom(Base58.decode(assetId.get)) else ByteString.EMPTY,
            transfers,
            Some(PBAttachment(PBAttachment.Attachment.BinaryValue(attachment)))
          )
        )
      )
      val proofs = crypto.sign(sender, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
      transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
    }

    def broadcastLease(source: KeyPair, recipient: PBRecipient, amount: Long, fee: Long, version: Int = 2): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(source.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        System.currentTimeMillis,
        version,
        PBTransaction.Data.Lease(LeaseTransactionData.of(Some(recipient), amount))
      )
      val proofs = crypto.sign(source, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
      transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
    }

    def broadcastLeaseCancel(source: KeyPair, leaseId: String, fee: Long, version: Int = 2): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(source.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        System.currentTimeMillis,
        version,
        PBTransaction.Data.LeaseCancel(LeaseCancelTransactionData.of(ByteString.copyFrom(Base58.decode(leaseId))))
      )
      val proofs = crypto.sign(source, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
      transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
    }

    def setAssetScript(
        sender: KeyPair,
        assetId: String,
        script: Option[String],
        fee: Long,
        timestamp: Long = System.currentTimeMillis(),
        version: Int = 1
    ): Future[PBSignedTransaction] = {
      val unsigned = PBTransaction(
        chainId,
        ByteString.copyFrom(sender.publicKey),
        Some(Amount.of(ByteString.EMPTY, fee)),
        timestamp,
        version,
        PBTransaction.Data.SetAssetScript(SetAssetScriptTransactionData.of(ByteString.copyFrom(Base58.decode(assetId)), toPBScriptOption(script)))
      )

      val proofs = crypto.sign(sender, PBTransactions.vanilla(SignedTransaction(Some(unsigned)), unsafe = true).explicitGet().bodyBytes())
      transactions.broadcast(SignedTransaction.of(Some(unsigned), Seq(ByteString.copyFrom(proofs))))
    }
  }
}
