package com.wavesplatform.it.api

import java.io.IOException
import java.net.InetSocketAddress
import java.util.concurrent.TimeoutException

import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.features.api.ActivationStatus
import com.wavesplatform.http.api_key
import com.wavesplatform.it.Node
import com.wavesplatform.it.util.GlobalTimer.{instance => timer}
import com.wavesplatform.it.util._
import com.wavesplatform.matcher.api.CancelOrderRequest
import com.wavesplatform.state.{ByteStr, DataEntry, Portfolio}
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.scalactic.source.Position
import org.scalatest.{Assertion, Assertions, Matchers}
import play.api.libs.json.Json.{parse, stringify, toJson}
import play.api.libs.json._
import scorex.api.http.PeersApiRoute.{ConnectReq, connectFormat}
import scorex.api.http.alias.CreateAliasV1Request
import scorex.api.http.assets._
import scorex.api.http.leasing.{LeaseCancelV1Request, LeaseV1Request, SignedLeaseCancelV1Request, SignedLeaseV1Request}
import scorex.api.http.{AddressApiRoute, ApiErrorResponse, DataRequest}
import scorex.transaction.transfer.MassTransferTransaction.Transfer
import scorex.transaction.assets.exchange.Order
import scorex.transaction.transfer._
import scorex.waves.http.DebugApiRoute._
import scorex.waves.http.DebugMessage._
import scorex.waves.http.{DebugMessage, RollbackParams}

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object AsyncHttpApi extends Assertions {

  case class ErrorMessage(error: Int, message: String)

  implicit val errorMessageFormat: Format[ErrorMessage] = Json.format

  def assertBadRequest(f: Future[_]): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, statusCode, _)) => Success(Assertions.assert(statusCode == StatusCodes.BadRequest.intValue))
    case Failure(e)                                               => Success(Assertions.fail(e))
    case _                                                        => Success(Assertions.fail(s"Expecting bad request"))
  }

  def expectErrorResponse(f: Future[_])(isExpected: ApiErrorResponse => Boolean): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      val parsedError = Json.parse(responseBody).validate[ApiErrorResponse].asOpt
      parsedError match {
        case None      => Success(Assertions.fail(s"Expecting bad request"))
        case Some(err) => Success(Assertions.assert(statusCode == StatusCodes.BadRequest.intValue && isExpected(err)))
      }
    case Failure(e) => Success(Assertions.fail(e))
    case _          => Success(Assertions.fail(s"Expecting bad request"))
  }

  def assertBadRequestAndMessage(f: Future[_], errorMessage: String): Future[Assertion] = f transform {
    case Failure(UnexpectedStatusCodeException(_, statusCode, responseBody)) =>
      Success(Assertions.assert(statusCode == StatusCodes.BadRequest.intValue && parse(responseBody).as[ErrorMessage].message.contains(errorMessage)))
    case Failure(e) => Success[Assertion](Assertions.fail(e))
    case _          => Success[Assertion](Assertions.fail(s"Expecting bad request"))
  }

  implicit class NodeAsyncHttpApi(n: Node) extends Assertions with Matchers {

    def matcherGet(path: String,
                   f: RequestBuilder => RequestBuilder = identity,
                   statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                   waitForStatus: Boolean = false): Future[Response] =
      retrying(f(_get(s"${n.matcherApiEndpoint}$path")).build(), statusCode = statusCode, waitForStatus = waitForStatus)

    def matcherGetWithSignature(path: String, ts: Long, signature: ByteStr, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
      retrying {
        _get(s"${n.matcherApiEndpoint}$path")
          .setHeader("Timestamp", ts)
          .setHeader("Signature", signature)
          .build()
      }

    def matcherGetStatusCode(path: String, statusCode: Int): Future[MessageMatcherResponse] =
      matcherGet(path, statusCode = statusCode).as[MessageMatcherResponse]

    def matcherPost[A: Writes](path: String, body: A): Future[Response] =
      post(s"${n.matcherApiEndpoint}$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

    def getOrderStatus(asset: String, orderId: String): Future[MatcherStatusResponse] =
      matcherGet(s"/matcher/orderbook/$asset/WAVES/$orderId", waitForStatus = true).as[MatcherStatusResponse]

    def getOrderBook(asset: String): Future[OrderBookResponse] =
      matcherGet(s"/matcher/orderbook/$asset/WAVES").as[OrderBookResponse]

    def getOrderbookByPublicKey(publicKey: String, timestamp: Long, signature: ByteStr): Future[Seq[OrderbookHistory]] =
      matcherGetWithSignature(s"/matcher/orderbook/$publicKey", timestamp, signature).as[Seq[OrderbookHistory]]

    def getOrderbookByPublicKeyActive(publicKey: String, timestamp: Long, signature: ByteStr): Future[Seq[OrderbookHistory]] =
      matcherGetWithSignature(s"/matcher/orderbook/$publicKey?activeOnly=true", timestamp, signature).as[Seq[OrderbookHistory]]

    def getReservedBalance(publicKey: String, timestamp: Long, signature: ByteStr): Future[Map[String, Long]] =
      matcherGetWithSignature(s"/matcher/balance/reserved/$publicKey", timestamp, signature).as[Map[String, Long]]

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

    def postJsObjectWithApiKey(path: String, body: JsObject): Future[Response] = retrying {
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
            case (_: IOException | _: TimeoutException) => Future(None)
          }

      def cond(ropt: Option[Response]) = ropt.exists { r =>
        r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200 && (Json.parse(r.getResponseBody) \ "height").as[Int] > 0
      }

      waitFor("node is up")(_ => send(), cond, 1.second)
    }

    def waitForPeers(targetPeersCount: Int): Future[Seq[Peer]] =
      waitFor[Seq[Peer]](s"connectedPeers.size >= $targetPeersCount")(_.connectedPeers, _.lengthCompare(targetPeersCount) >= 0, 1.second)

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

    def scriptInfo(address: String): Future[AddressApiRoute.AddressScriptInfo] =
      get(s"/addresses/scriptInfo/$address").as[AddressApiRoute.AddressScriptInfo]

    def findTransactionInfo(txId: String): Future[Option[TransactionInfo]] = transactionInfo(txId).transform {
      case Success(tx)                                       => Success(Some(tx))
      case Failure(UnexpectedStatusCodeException(_, 404, _)) => Success(None)
      case Failure(ex)                                       => Failure(ex)
    }

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second): Future[TransactionInfo] =
      waitFor[Option[TransactionInfo]](s"transaction $txId")(
        _.transactionInfo(txId).transform {
          case Success(tx)                                       => Success(Some(tx))
          case Failure(UnexpectedStatusCodeException(_, 404, _)) => Success(None)
          case Failure(ex)                                       => Failure(ex)
        },
        tOpt => tOpt.exists(_.id == txId),
        retryInterval
      ).map(_.get)

    def waitForUtxIncreased(fromSize: Int): Future[Int] = waitFor[Int](s"utxSize > $fromSize")(
      _.utxSize,
      _ > fromSize,
      100.millis
    )

    def waitForHeight(expectedHeight: Int): Future[Int] = waitFor[Int](s"height >= $expectedHeight")(_.height, h => h >= expectedHeight, 5.seconds)

    def transactionInfo(txId: String): Future[TransactionInfo] = get(s"/transactions/info/$txId").as[TransactionInfo]

    def effectiveBalance(address: String): Future[Balance] = get(s"/addresses/effectiveBalance/$address").as[Balance]

    def transfer(sourceAddress: String,
                 recipient: String,
                 amount: Long,
                 fee: Long,
                 assetId: Option[String] = None,
                 feeAssetId: Option[String] = None): Future[Transaction] =
      postJson("/assets/transfer", TransferV1Request(assetId, feeAssetId, amount, fee, sourceAddress, None, recipient)).as[Transaction]

    def payment(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
      postJson("/waves/payment", PaymentRequest(amount, fee, sourceAddress, recipient)).as[Transaction]

    def lease(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
      postJson("/leasing/lease", LeaseV1Request(sourceAddress, amount, fee, recipient)).as[Transaction]

    def cancelLease(sourceAddress: String, leaseId: String, fee: Long): Future[Transaction] =
      postJson("/leasing/cancel", LeaseCancelV1Request(sourceAddress, leaseId, fee)).as[Transaction]

    def activeLeases(sourceAddress: String) = get(s"/leasing/active/$sourceAddress").as[Seq[Transaction]]

    def issue(sourceAddress: String,
              name: String,
              description: String,
              quantity: Long,
              decimals: Byte,
              reissuable: Boolean,
              fee: Long): Future[Transaction] =
      postJson("/assets/issue", IssueV1Request(sourceAddress, name, description, quantity, decimals, reissuable, fee)).as[Transaction]

    def scriptCompile(code: String) = post("/utils/script/compile", code).as[CompiledScript]

    def reissue(sourceAddress: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long): Future[Transaction] =
      postJson("/assets/reissue", ReissueV1Request(sourceAddress, assetId, quantity, reissuable, fee)).as[Transaction]

    def burn(sourceAddress: String, assetId: String, quantity: Long, fee: Long): Future[Transaction] =
      postJson("/assets/burn", BurnV1Request(sourceAddress, assetId, quantity, fee)).as[Transaction]

    def assetBalance(address: String, asset: String): Future[AssetBalance] =
      get(s"/assets/balance/$address/$asset").as[AssetBalance]

    def assetsBalance(address: String): Future[FullAssetsInfo] =
      get(s"/assets/balance/$address").as[FullAssetsInfo]

    def assetsDetails(assetId: String): Future[AssetInfo] =
      get(s"/assets/details/$assetId").as[AssetInfo]

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

    def sign(jsobj: JsObject): Future[JsObject] =
      postJsObjectWithApiKey("/transactions/sign", jsobj).as[JsObject]

    def signedBroadcast(jsobj: JsObject): Future[Transaction] =
      post("/transactions/broadcast", stringify(jsobj)).as[Transaction]

    def signAndBroadcast(jsobj: JsObject): Future[Transaction] = sign(jsobj).flatMap(signedBroadcast)

    def signedIssue(issue: SignedIssueV1Request): Future[Transaction] =
      postJson("/assets/broadcast/issue", issue).as[Transaction]

    def batchSignedTransfer(transfers: Seq[SignedTransferV1Request], timeout: FiniteDuration = 1.minute): Future[Seq[Transaction]] = {
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

    def waitForNextBlock: Future[Block] =
      for {
        currentBlock <- lastBlock
        actualBlock  <- findBlock(_.height > currentBlock.height, currentBlock.height)
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

    def placeOrder(order: Order): Future[MatcherResponse] =
      matcherPost("/matcher/orderbook", order.json()).as[MatcherResponse]

    def expectIncorrectOrderPlacement(order: Order, expectedStatusCode: Int, expectedStatus: String): Future[Boolean] =
      matcherPost("/matcher/orderbook", order.json()) transform {
        case Failure(UnexpectedStatusCodeException(_, `expectedStatusCode`, responseBody)) =>
          Try(parse(responseBody).as[MatcherStatusResponse]) match {
            case Success(mr) if mr.status == expectedStatus => Success(true)
            case Failure(f)                                 => Failure(new RuntimeException(s"Failed to parse response: $f"))
          }
        case Success(r) => Failure(new RuntimeException(s"Unexpected matcher response: (${r.getStatusCode}) ${r.getResponseBody}"))
        case _          => Failure(new RuntimeException(s"Unexpected failure from matcher"))
      }

    def cancelOrder(amountAsset: String, priceAsset: String, request: CancelOrderRequest): Future[MatcherStatusResponse] =
      matcherPost(s"/matcher/orderbook/$amountAsset/$priceAsset/cancel", request.json).as[MatcherStatusResponse]

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
                  n.log.debug(s"Request: ${r.getUrl}\nResponse: ${response.getResponseBody}")
                  response
                } else {
                  n.log.debug(s"Request: ${r.getUrl}\nUnexpected status code(${response.getStatusCode}): ${response.getResponseBody}")
                  throw UnexpectedStatusCodeException(r.getUrl, response.getStatusCode, response.getResponseBody)
                }
              }
            }
          )
          .toCompletableFuture
          .toScala
          .recoverWith {
            case e: UnexpectedStatusCodeException if waitForStatus =>
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
      n.log.debug(s"Request: ${r.getUrl}")
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

  }

  implicit class NodesAsyncHttpApi(nodes: Seq[Node]) extends Matchers {
    def waitForHeightAriseAndTxPresent(transactionId: String)(implicit p: Position): Future[Unit] =
      for {
        allHeights   <- traverse(nodes)(_.waitForTransaction(transactionId).map(_.height))
        _            <- traverse(nodes)(_.waitForHeight(allHeights.max + 2))
        finalHeights <- traverse(nodes)(_.waitForTransaction(transactionId).map(_.height))
      } yield all(finalHeights).shouldBe(finalHeights.head)

    def waitForHeightArise(): Future[Unit] =
      for {
        height <- traverse(nodes)(_.height).map(_.max)
        _      <- traverse(nodes)(_.waitForHeight(height + 1))
      } yield ()

    def waitForSameBlocksAt(height: Int, retryInterval: FiniteDuration = 5.seconds): Future[Boolean] = {

      def waitHeight = waitFor[Int](s"all heights >= $height")(retryInterval)(_.height, _.forall(_ >= height))

      def waitSameBlocks =
        waitFor[Block](s"same blocks at height = $height")(retryInterval)(_.blockAt(height), { blocks =>
          val sig = blocks.map(_.signature)
          sig.forall(_ == sig.head)
        })

      for {
        _ <- waitHeight
        r <- waitSameBlocks
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
