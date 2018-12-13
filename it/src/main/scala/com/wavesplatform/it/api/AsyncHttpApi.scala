package com.wavesplatform.it.api

import java.io.IOException
import java.net.InetSocketAddress
import java.util.concurrent.TimeoutException

import com.wavesplatform.api.http.alias.CreateAliasV1Request
import com.wavesplatform.api.http.assets._
import com.wavesplatform.api.http.leasing.{LeaseCancelV1Request, LeaseV1Request, SignedLeaseCancelV1Request, SignedLeaseV1Request}
import com.wavesplatform.api.http.{AddressApiRoute, ConnectReq, DataRequest}
import com.wavesplatform.features.api.ActivationStatus
import com.wavesplatform.http.DebugApiRoute._
import com.wavesplatform.http.DebugMessage._
import com.wavesplatform.http.{DebugMessage, RollbackParams, api_key}
import com.wavesplatform.it.Node
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import com.wavesplatform.state.{DataEntry, EitherExt2, Portfolio}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer._
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
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

  implicit class NodeAsyncHttpApi(n: Node) extends Assertions with Matchers {

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

    def post(url: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying(f(_post(url).withApiKey(n.apiKey)).build())

    def postJson[A: Writes](path: String, body: A): Future[Response] =
      post(path, stringify(toJson(body)))

    def post(path: String, body: String): Future[Response] =
      post(s"${n.nodeApiEndpoint}$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json;charset=utf-8").setBody(body))

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
        r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200 && (Json.parse(r.getResponseBody) \ "height").as[Int] > 0
      }

      waitFor("node is up")(_ => send(), cond, 1.second)
    }

    def waitForPeers(targetPeersCount: Int): Future[Seq[Peer]] =
      waitFor[Seq[Peer]](s"connectedPeers.size >= $targetPeersCount")(_.connectedPeers, _.lengthCompare(targetPeersCount) >= 0, 1.second)

    def waitForBlackList(blackListSize: Int): Future[Seq[BlacklistedPeer]] =
      waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers > $blackListSize")(_.blacklistedPeers, _.lengthCompare(blackListSize) > 0, 500.millis)

    def height: Future[Int] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Int])

    def blockAt(height: Int) = get(s"/blocks/at/$height").as[Block]

    def utx = get(s"/transactions/unconfirmed").as[Seq[Transaction]]

    def utxSize = get(s"/transactions/unconfirmed/size").as[JsObject].map(_.value("size").as[Int])

    def lastBlock: Future[Block] = get("/blocks/last").as[Block]

    def blockSeq(from: Int, to: Int) = get(s"/blocks/seq/$from/$to").as[Seq[Block]]

    def blockHeadersAt(height: Int) = get(s"/blocks/headers/at/$height").as[BlockHeaders]

    def blockHeadersSeq(from: Int, to: Int) = get(s"/blocks/headers/seq/$from/$to").as[Seq[BlockHeaders]]

    def lastBlockHeaders: Future[BlockHeaders] = get("/blocks/headers/last").as[BlockHeaders]

    def status: Future[Status] = get("/node/status").as[Status]

    def activationStatus: Future[ActivationStatus] = get("/activation/status").as[ActivationStatus]

    def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]

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

    def transactionsByAddress(address: String, limit: Int): Future[Seq[Seq[TransactionInfo]]] =
      get(s"/transactions/address/$address/limit/$limit").as[Seq[Seq[TransactionInfo]]]

    def effectiveBalance(address: String): Future[Balance] = get(s"/addresses/effectiveBalance/$address").as[Balance]

    def transfer(sourceAddress: String,
                 recipient: String,
                 amount: Long,
                 fee: Long,
                 assetId: Option[String] = None,
                 feeAssetId: Option[String] = None,
                 version: Byte = 2): Future[Transaction] = {
      version match {
        case 2 =>
          postJson("/assets/transfer", TransferV2Request(version, assetId, amount, feeAssetId, fee, sourceAddress, None, recipient)).as[Transaction]
        case _ => postJson("/assets/transfer", TransferV1Request(assetId, feeAssetId, amount, fee, sourceAddress, None, recipient)).as[Transaction]
      }
    }

    def payment(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
      postJson("/waves/payment", PaymentRequest(amount, fee, sourceAddress, recipient)).as[Transaction]

    def lease(sourceAddress: String, recipient: String, amount: Long, fee: Long, version: Byte = 2): Future[Transaction] = {
      version match {
        case 2 => { //TODO: @monroid refactor after https://wavesplatform.atlassian.net/browse/NODE-1222 fix
          signAndBroadcast(
            Json.obj(
              "type"      -> 8,
              "sender"    -> sourceAddress,
              "amount"    -> amount,
              "recipient" -> recipient,
              "fee"       -> fee,
              "version"   -> version
            ))
        }
        case _ => postJson("/leasing/lease", LeaseV1Request(sourceAddress, amount, fee, recipient)).as[Transaction]
      }
    }

    def cancelLease(sourceAddress: String, leaseId: String, fee: Long, version: Byte = 2): Future[Transaction] = {
      version match {
        case 2 => { //TODO: @monroid refactor after https://wavesplatform.atlassian.net/browse/NODE-1222 fix
          signAndBroadcast(
            Json.obj(
              "type"    -> 9,
              "sender"  -> sourceAddress,
              "txId"    -> leaseId,
              "fee"     -> fee,
              "version" -> version
            ))
        }
        case _ => postJson("/leasing/cancel", LeaseCancelV1Request(sourceAddress, leaseId, fee)).as[Transaction]
      }
    }

    def activeLeases(sourceAddress: String) = get(s"/leasing/active/$sourceAddress").as[Seq[Transaction]]

    def issue(sourceAddress: String,
              name: String,
              description: String,
              quantity: Long,
              decimals: Byte,
              reissuable: Boolean,
              fee: Long,
              version: Byte = 2,
              script: Option[String] = None): Future[Transaction] = {
      version match {
        case 2 => { //TODO: @monroid refactor after https://wavesplatform.atlassian.net/browse/NODE-1222 fix
          val js = Json.obj(
            "type"        -> 3,
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
        case _ => postJson("/assets/issue", IssueV1Request(sourceAddress, name, description, quantity, decimals, reissuable, fee)).as[Transaction]
      }
    }

    def setAssetScript(assetId: String, sender: String, fee: Long, script: Option[String] = None): Future[Transaction] = {
      signAndBroadcast(
        Json.obj(
          "type"    -> 15,
          "version" -> 1,
          "assetId" -> assetId,
          "sender"  -> sender,
          "fee"     -> fee,
          "script"  -> script
        ))
    }

    def scriptCompile(code: String) = post("/utils/script/compile", code).as[CompiledScript]

    def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Future[Transaction] =
      postJson("/assets/reissue", ReissueV1Request(sourceAddress, assetId, quantity, reissuable, fee)).as[Transaction]

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long): Future[Transaction] =
      postJson("/assets/burn", BurnV1Request(sourceAddress, assetId, quantity, fee)).as[Transaction]

    def assetBalance(address: String, asset: String): Future[AssetBalance] =
      get(s"/assets/balance/$address/$asset").as[AssetBalance]

    def assetsBalance(address: String): Future[FullAssetsInfo] =
      get(s"/assets/balance/$address").as[FullAssetsInfo]

    def assetsDetails(assetId: String, fullInfo: Boolean = false): Future[AssetInfo] =
      get(s"/assets/details/$assetId?full=$fullInfo").as[AssetInfo]

    def sponsorAsset(sourceAddress: String, assetId: String, minSponsoredAssetFee: Long, fee: Long): Future[Transaction] =
      postJson("/assets/sponsor", SponsorFeeRequest(1, sourceAddress, assetId, Some(minSponsoredAssetFee), fee)).as[Transaction]

    def cancelSponsorship(sourceAddress: String, assetId: String, fee: Long): Future[Transaction] =
      postJson("/assets/sponsor", SponsorFeeRequest(1, sourceAddress, assetId, None, fee)).as[Transaction]

    def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
      postJson("/assets/transfer", TransferV1Request(None, None, amount, fee, sourceAddress, None, recipient)).as[Transaction]

    def massTransfer(sourceAddress: String, transfers: List[Transfer], fee: Long, assetId: Option[String] = None): Future[Transaction] = {
      implicit val w: Writes[MassTransferRequest] = Json.writes[MassTransferRequest]
      postJson("/assets/masstransfer", MassTransferRequest(MassTransferTransaction.version, assetId, sourceAddress, transfers, fee, None))
        .as[Transaction]
    }

    def putData(sourceAddress: String, data: List[DataEntry[_]], fee: Long): Future[Transaction] = {
      implicit val w: Writes[DataRequest] = Json.writes[DataRequest]
      postJson("/addresses/data", DataRequest(1, sourceAddress, data, fee)).as[Transaction]
    }

    def getData(address: String): Future[List[DataEntry[_]]] = get(s"/addresses/data/$address").as[List[DataEntry[_]]]

    def getData(address: String, key: String): Future[DataEntry[_]] = get(s"/addresses/data/$address/$key").as[DataEntry[_]]

    def signedTransfer(transfer: SignedTransferV1Request): Future[Transaction] =
      postJson("/assets/broadcast/transfer", transfer).as[Transaction]

    def signedLease(lease: SignedLeaseV1Request): Future[Transaction] =
      postJson("/leasing/broadcast/lease", lease).as[Transaction]

    def signedLeaseCancel(leaseCancel: SignedLeaseCancelV1Request): Future[Transaction] =
      postJson("/leasing/broadcast/cancel", leaseCancel).as[Transaction]

    def broadcastRequest[A: Writes](req: A): Future[Transaction] = postJson("/transactions/broadcast", req).as[Transaction]

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

    def signAndBroadcast(json: JsValue): Future[Transaction] = sign(json).flatMap(signedBroadcast)

    def signedIssue(issue: SignedIssueV1Request): Future[Transaction] =
      postJson("/assets/broadcast/issue", issue).as[Transaction]

    def signedIssue(issue: SignedIssueV2Request): Future[Transaction] =
      signedBroadcast(issue.toTx.explicitGet().json())

    def batchSignedTransfer(transfers: Seq[SignedTransferV2Request], timeout: FiniteDuration = 1.minute): Future[Seq[Transaction]] = {
      val request = _post(s"${n.nodeApiEndpoint}/assets/broadcast/batch-transfer")
        .setHeader("Content-type", "application/json")
        .withApiKey(n.apiKey)
        .setReadTimeout(timeout.toMillis.toInt)
        .setRequestTimeout(timeout.toMillis.toInt)
        .setBody(stringify(toJson(transfers)))
        .build()

      def aux: Future[Response] =
        once(request)
          .flatMap { response =>
            if (response.getStatusCode == 503) throw new IOException(s"Unexpected status code: 503")
            else Future.successful(response)
          }
          .recoverWith {
            case e @ (_: IOException | _: TimeoutException) =>
              n.log.debug(s"Failed to send ${transfers.size} txs: ${e.getMessage}")
              timer.schedule(aux, 20.seconds)
          }

      aux.as[Seq[Transaction]]
    }

    def createAlias(targetAddress: String, alias: String, fee: Long): Future[Transaction] =
      postJson("/alias/create", CreateAliasV1Request(targetAddress, alias, fee)).as[Transaction]

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

    def issueAsset(address: String,
                   name: String,
                   description: String,
                   quantity: Long,
                   decimals: Byte,
                   fee: Long,
                   reissuable: Boolean): Future[Transaction] =
      postJson("/assets/issue", IssueV1Request(address, name, description, quantity, decimals, reissuable, fee)).as[Transaction]

    def retrying(r: Request,
                 interval: FiniteDuration = 1.second,
                 statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                 waitForStatus: Boolean = false): Future[Response] = {
      def executeRequest: Future[Response] = {
        n.log.trace(s"Executing request '$r'")
        if (r.getStringData != null) n.log.debug(s"Request's body '${r.getStringData}'")
        n.client
          .executeRequest(
            r,
            new AsyncCompletionHandler[Response] {
              override def onCompleted(response: Response): Response = {
                if (response.getStatusCode == statusCode) {
                  n.log.debug(s"Request: ${r.getMethod} ${r.getUrl}\nResponse: ${response.getResponseBody}")
                  response
                } else {
                  n.log.debug(s"Request: ${r.getMethod} ${r.getUrl}\nUnexpected status code(${response.getStatusCode}): ${response.getResponseBody}")
                  throw UnexpectedStatusCodeException(r.getMethod, r.getUrl, response.getStatusCode, response.getResponseBody)
                }
              }
            }
          )
          .toCompletableFuture
          .toScala
          .recoverWith {
            case e: UnexpectedStatusCodeException if e.statusCode == 503 || waitForStatus =>
              n.log.debug(s"Failed to execute request '$r' with error: ${e.getMessage}")
              timer.schedule(executeRequest, interval)
            case e @ (_: IOException | _: TimeoutException) =>
              n.log.debug(s"Failed to execute request '$r' with error: ${e.getMessage}")
              timer.schedule(executeRequest, interval)
          }
      }

      executeRequest
    }

    def once(r: Request): Future[Response] = {
      n.log.debug(s"Request: ${r.getMethod} ${r.getUrl}")
      n.client
        .executeRequest(
          r,
          new AsyncCompletionHandler[Response] {
            override def onCompleted(response: Response): Response = {
              n.log.debug(s"Response for ${r.getUrl} is ${response.getStatusCode}")
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

    def accountEffectiveBalance(acc: String): Future[Long] = n.effectiveBalance(acc).map(_.balance)

    def accountBalance(acc: String): Future[Long] = n.balance(acc).map(_.balance)

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
      } yield all(finalHeights) should be >= (finalHeights.head)

    def waitForTransaction(transactionId: String)(implicit p: Position): Future[TransactionInfo] =
      traverse(nodes)(_.waitForTransaction(transactionId)).map(_.head)

    def waitForHeightArise(): Future[Int] =
      for {
        height <- height.map(_.max)
        _      <- traverse(nodes)(_.waitForHeight(height + 1))
      } yield height + 1

    def waitForSameBlockHeadesAt(height: Int, retryInterval: FiniteDuration = 5.seconds): Future[Boolean] = {

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
    def withApiKey(x: String): RequestBuilder = self.setHeader(api_key.name, x)
  }

}
