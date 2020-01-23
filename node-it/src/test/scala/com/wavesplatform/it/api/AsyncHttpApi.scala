package com.wavesplatform.it.api

import java.io.IOException
import java.net.{InetSocketAddress, URLEncoder}
import java.nio.charset.StandardCharsets
import java.util.{NoSuchElementException, UUID}
import java.util.concurrent.TimeoutException

import com.wavesplatform.api.http.RewardApiRoute.RewardStatus
import com.wavesplatform.api.http.assets._
import com.wavesplatform.api.http.{AddressApiRoute, ConnectReq}
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.features.api.ActivationStatus
import com.wavesplatform.http.DebugMessage._
import com.wavesplatform.http.{DebugMessage, RollbackParams, `X-Api-Key`}
import com.wavesplatform.it.Node
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.{FunctionHeader, Serde}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.FUNCTION_CALL
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, DataEntry, Portfolio}
import com.wavesplatform.transaction.assets.{BurnTransaction, IssueTransaction, SetAssetScriptTransaction, SponsorFeeTransaction}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{CreateAliasTransaction, DataTransaction}
import org.asynchttpclient.Dsl.{get => _get, post => _post, put => _put}
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

    def utx: Future[Seq[Transaction]] = get(s"/transactions/unconfirmed").as[Seq[Transaction]]

    def utxSize: Future[Int] = get(s"/transactions/unconfirmed/size").as[JsObject].map(_.value("size").as[Int])

    def lastBlock: Future[Block] = get("/blocks/last").as[Block]

    def blockSeq(from: Int, to: Int): Future[Seq[Block]] = get(s"/blocks/seq/$from/$to").as[Seq[Block]]

    def blockSeqByAddress(address: String, from: Int, to: Int): Future[Seq[Block]] = get(s"/blocks/address/$address/$from/$to").as[Seq[Block]]

    def blockHeadersAt(height: Int): Future[BlockHeaders] = get(s"/blocks/headers/at/$height").as[BlockHeaders]

    def blockHeadersSeq(from: Int, to: Int): Future[Seq[BlockHeaders]] = get(s"/blocks/headers/seq/$from/$to").as[Seq[BlockHeaders]]

    def lastBlockHeaders: Future[BlockHeaders] = get("/blocks/headers/last").as[BlockHeaders]

    def status: Future[Status] = get("/node/status").as[Status]

    def activationStatus: Future[ActivationStatus] = get("/activation/status").as[ActivationStatus]

    def rewardStatus(height: Int): Future[RewardStatus] = get(s"/blockchain/rewards/$height").as[RewardStatus]

    def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]

    def balances(height: Option[Int], addresses: Seq[String], asset: Option[String]): Future[Seq[Balance]] = {
      for {
        json <- postJson(
          "/addresses/balance",
          Json.obj("addresses" -> addresses) ++
            height.fold(Json.obj())(h => Json.obj("height" -> h)) ++
            asset.fold(Json.obj())(a => Json.obj("asset" -> a))
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


    def transactionsStatus(txIds:Seq[String]):Future[Seq[TransactionStatus]] =
      postJson(s"/transactions/status", Json.obj("ids" -> txIds)).as[List[TransactionStatus]]

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
        version: Byte = 2
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

    def lease(sourceAddress: String, recipient: String, amount: Long, fee: Long, version: Byte = 2): Future[Transaction] = {
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

    def cancelLease(sourceAddress: String, leaseId: String, fee: Long, version: Byte = 2): Future[Transaction] = {
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
        version: Byte = 2,
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

    def setScript(sender: String, script: Option[String] = None, fee: Long = 1000000, version: Byte = 1): Future[Transaction] = {
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

    def setAssetScript(assetId: String, sender: String, fee: Long, script: Option[String] = None, version: Byte = 1): Future[Transaction] = {
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
        version: Byte = 1
    ): Future[(Transaction, JsValue)] = {
      signAndTraceBroadcast(
        Json.obj(
          "type"       -> InvokeScriptTransaction.typeId,
          "version"    -> version,
          "sender"     -> caller,
          "dApp"       -> dappAddress,
          "call"       -> { if (func.isDefined) InvokeScriptTransaction.functionCallToJson(FUNCTION_CALL(FunctionHeader.User(func.get), args)) else JsNull },
          "payment"    -> payment,
          "fee"        -> fee,
          "feeAssetId" -> { if (feeAssetId.isDefined) JsString(feeAssetId.get) else JsNull }
        )
      )
    }

    def scriptCompile(code: String): Future[CompiledScript] = post("/utils/script/compile", code).as[CompiledScript]

    def scriptDecompile(script: String): Future[DecompiledScript] = post("/utils/script/decompile", script).as[DecompiledScript]

    def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Future[Transaction] =
      postJson("/assets/reissue", ReissueV1Request(sourceAddress, assetId, quantity, reissuable, fee)).as[Transaction]

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long, version: Byte = 2): Future[Transaction] = {
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
      postJson("/assets/transfer", TransferV1Request(None, None, amount, fee, sourceAddress, None, recipient)).as[Transaction]

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

    def getData(address: String): Future[List[DataEntry[_]]] = get(s"/addresses/data/$address").as[List[DataEntry[_]]]

    def getData(address: String, regexp: String): Future[List[DataEntry[_]]] = get(s"/addresses/data/$address?matches=$regexp").as[List[DataEntry[_]]]

    def getDataByKey(address: String, key: String): Future[DataEntry[_]] = get(s"/addresses/data/$address/$key").as[DataEntry[_]]

    def getDataListJson(address: String, keys: String*): Future[Seq[DataEntry[_]]] =
      postJson(s"/addresses/data/$address", Json.obj("keys" -> keys)).as[Seq[DataEntry[_]]]

    def getDataListPost(address: String, keys: String*): Future[Seq[DataEntry[_]]] =
      postForm(s"/addresses/data/$address", keys.map("key" -> URLEncoder.encode(_, "UTF-8")): _*).as[Seq[DataEntry[_]]]

    def getDataList(address: String, keys: String*): Future[Seq[DataEntry[_]]] =
      get(s"/addresses/data/$address?${keys.map("key=" + URLEncoder.encode(_, "UTF-8")).mkString("&")}").as[Seq[DataEntry[_]]]

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

    def signedIssue(issue: SignedIssueV1Request): Future[Transaction] =
      postJson("/assets/broadcast/issue", issue).as[Transaction]

    def signedIssue(issue: SignedIssueV2Request): Future[Transaction] =
      signedBroadcast(issue.toTx.explicitGet().json())

    def batchSignedTransfer(transfers: Seq[SignedTransferV2Request], timeout: FiniteDuration = 1.minute): Future[Seq[Transaction]] = {
      import SignedTransferV2Request.writes
      Future.sequence(transfers.map(v => signedBroadcast(toJson(v).as[JsObject] ++ Json.obj("type" -> TransferTransaction.typeId.toInt))))
    }

    def createAlias(targetAddress: String, alias: String, fee: Long, version: Byte = 2): Future[Transaction] =
      signAndBroadcast(
        Json.obj(
          "type"    -> CreateAliasTransaction.typeId,
          "version" -> version,
          "sender"  -> targetAddress,
          "fee"     -> fee,
          "alias"   -> alias
        )
      )

    def aliasByAddress(targetAddress: String): Future[Seq[String]] =
      get(s"/alias/by-address/$targetAddress").as[Seq[String]]

    def addressByAlias(targetAlias: String): Future[Address] =
      get(s"/alias/by-alias/$targetAlias").as[Address]

    def rollback(to: Int, returnToUTX: Boolean = true): Future[Unit] =
      postJson("/debug/rollback", RollbackParams(to, returnToUTX)).map(_ => ())

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
      n.balances(height,accounts, asset).map(_.map(b => (b.address, b.balance)))
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
}
