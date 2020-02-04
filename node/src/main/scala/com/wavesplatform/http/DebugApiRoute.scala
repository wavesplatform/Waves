package com.wavesplatform.http

import java.net.{InetAddress, InetSocketAddress, URI}
import java.util.concurrent.ConcurrentMap

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import cats.implicits._
import cats.kernel.Monoid
import com.typesafe.config.{ConfigObject, ConfigRenderOptions}
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.ApiError.InvalidAddress
import com.wavesplatform.api.http._
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{Miner, MinerDebugInfo}
import com.wavesplatform.network.{PeerDatabase, PeerInfo, _}
import com.wavesplatform.settings.{RestAPISettings, WavesSettings}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.extensions.Distributions
import com.wavesplatform.state.{Blockchain, LeaseBalance, NG, TransactionId}
import com.wavesplatform.transaction.TxValidationError.InvalidRequestSignature
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, Verifier}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import play.api.libs.json._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

case class DebugApiRoute(
    ws: WavesSettings,
    time: Time,
    blockchain: Blockchain,
    wallet: Wallet,
    ng: NG,
    peerDatabase: PeerDatabase,
    establishedConnections: ConcurrentMap[Channel, PeerInfo],
    rollbackTask: (ByteStr, Boolean) => Task[Either[ValidationError, Unit]],
    utxStorage: UtxPool,
    miner: Miner with MinerDebugInfo,
    historyReplier: HistoryReplier,
    extLoaderStateReporter: Coeval[RxExtensionLoader.State],
    mbsCacheSizesReporter: Coeval[MicroBlockSynchronizer.CacheSizes],
    scoreReporter: Coeval[RxScoreObserver.Stats],
    configRoot: ConfigObject,
    loadBalanceHistory: Address => Seq[(Int, Long)]
) extends ApiRoute
    with AuthRoute
    with ScorexLogging {

  import DebugApiRoute._

  private[this] val dst                  = Distributions(ng)
  private lazy val configStr             = configRoot.render(ConfigRenderOptions.concise().setJson(true).setFormatted(true))
  private lazy val fullConfig: JsValue   = Json.parse(configStr)
  private lazy val wavesConfig: JsObject = Json.obj("waves" -> (fullConfig \ "waves").get)

  override val settings: RestAPISettings = ws.restAPISettings
  override lazy val route: Route = pathPrefix("debug") {
    stateChanges ~ balanceHistory ~ withAuth {
      blocks ~ state ~ info ~ stateWaves ~ rollback ~ rollbackTo ~ blacklist ~ portfolios ~ minerInfo ~ historyInfo ~ configInfo ~ print ~ validate
    }
  }

  def blocks: Route = {
    (path("blocks" / IntNumber) & get) { howMany =>
      complete(JsArray(ng.lastBlocks(howMany).map { block =>
        val bytes = block.bytes()
        Json.obj(bytes.length.toString -> Base58.encode(crypto.fastHash(bytes)))
      }))
    }
  }

  def print: Route =
    path("print")(jsonPost[DebugMessage] { params =>
      log.debug(params.message.take(250))
      ""
    })

  def portfolios: Route = path("portfolios" / Segment) { rawAddress =>
    (get & parameter('considerUnspent.as[Boolean].?)) { considerUnspent =>
      Address.fromString(rawAddress) match {
        case Left(_) => complete(InvalidAddress)
        case Right(address) =>
          val base      = dst.portfolio(address)
          val portfolio = if (considerUnspent.getOrElse(true)) Monoid.combine(base, utxStorage.pessimisticPortfolio(address)) else base
          complete(Json.toJson(portfolio))
      }
    }
  }

  def balanceHistory: Route = (path("balances" / "history" / AddrSegment) & get) { address =>
    complete(Json.toJson(loadBalanceHistory(address).map {
      case (h, b) => Json.obj("height" -> h, "balance" -> b)
    }))
  }

  private def wavesDistribution(height: Int): Route =
    optionalHeaderValueByType[Accept](()) {
      case Some(accept) if accept.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings) =>
        complete(dst.wavesDistribution(height).map(_.map { case (a, b) => a.stringRepr -> b.toString }))
      case _ =>
        complete(dst.wavesDistribution(height).map(_.map { case (a, b) => a.stringRepr -> b }))
    }

  def state: Route = (path("state") & get) {
    wavesDistribution(ng.height)
  }

  def stateWaves: Route = (path("stateWaves" / IntNumber) & get) { height =>
    wavesDistribution(height)
  }

  private def rollbackToBlock(blockId: ByteStr, returnTransactionsToUtx: Boolean)(
      implicit ec: ExecutionContext
  ): Future[Either[ValidationError, JsObject]] = {
    rollbackTask(blockId, returnTransactionsToUtx)
      .map(_ => Right(Json.obj("BlockId" -> blockId.toString)))
      .runAsyncLogErr(Scheduler(ec))
  }

  def rollback: Route = (path("rollback") & withRequestTimeout(15.minutes) & extractScheduler) { implicit sc =>
    jsonPost[RollbackParams] { params =>
      ng.blockAt(params.rollbackTo) match {
        case Some(block) =>
          rollbackToBlock(block.uniqueId, params.returnTransactionsToUtx)
        case None =>
          (StatusCodes.BadRequest, "Block at height not found")
      }
    } ~ complete(StatusCodes.BadRequest)
  }

  def info: Route = (path("info") & get) {
    complete(
      Json.obj(
        "stateHeight"                      -> ng.height,
        "extensionLoaderState"             -> extLoaderStateReporter().toString,
        "historyReplierCacheSizes"         -> Json.toJson(historyReplier.cacheSizes),
        "microBlockSynchronizerCacheSizes" -> Json.toJson(mbsCacheSizesReporter()),
        "scoreObserverStats"               -> Json.toJson(scoreReporter()),
        "minerState"                       -> Json.toJson(miner.state)
      )
    )
  }

  def minerInfo: Route = (path("minerInfo") & get) {
    complete(
      wallet.privateKeyAccounts
        .filterNot(account => ng.hasScript(account.toAddress))
        .map { account =>
          (account.toAddress, miner.getNextBlockGenerationOffset(account))
        }
        .collect {
          case (address, Right(offset)) =>
            AccountMiningInfo(
              address.stringRepr,
              ng.effectiveBalance(
                address,
                ws.blockchainSettings.functionalitySettings.generatingBalanceDepth(ng.height),
                ng.microblockIds.lastOption
              ),
              System.currentTimeMillis() + offset.toMillis
            )
        }
    )
  }

  def historyInfo: Route = (path("historyInfo") & get) {
    val a = ng.lastPersistedBlockIds(10)
    val b = ng.microblockIds
    complete(HistoryInfo(a, b))

  }

  def configInfo: Route = (path("configInfo") & get & parameter('full.as[Boolean])) { full =>
    complete(if (full) fullConfig else wavesConfig)
  }

  def rollbackTo: Route = path("rollback-to" / Segment) { signature =>
    (delete & extractScheduler) { implicit sc =>
      val signatureEi: Either[ValidationError, ByteStr] =
        ByteStr
          .decodeBase58(signature)
          .toEither
          .leftMap(_ => InvalidRequestSignature)
      signatureEi
        .fold(
          err => complete(ApiError.fromValidationError(err)),
          sig => complete(rollbackToBlock(sig, returnTransactionsToUtx = false))
        )
    }
  }

  def blacklist: Route = (path("blacklist") & post) {
    entity(as[String]) { socketAddressString =>
      try {
        val uri     = new URI("node://" + socketAddressString)
        val address = InetAddress.getByName(uri.getHost)
        establishedConnections.entrySet().stream().forEach { entry =>
          entry.getValue.remoteAddress match {
            case x: InetSocketAddress if x.getAddress == address =>
              peerDatabase.blacklistAndClose(entry.getKey, "Debug API request")
            case _ =>
          }
        }
        peerDatabase.blacklist(address, "Debug API request")
        complete(StatusCodes.OK)
      } catch {
        case NonFatal(_) => complete(StatusCodes.BadRequest)
      }
    } ~ complete(StatusCodes.BadRequest)
  }

  def validate: Route =
    path("validate")(jsonPost[JsObject] { jsv =>
      val h  = blockchain.height
      val t0 = System.nanoTime
      val tracedDiff = for {
        tx <- TracedResult(TransactionFactory.fromSignedRequest(jsv))
        _  <- Verifier(blockchain, h)(tx)
        ei <- TransactionDiffer(blockchain.lastBlockTimestamp, time.correctedTime(), h)(blockchain, tx)
      } yield ei
      val timeSpent = (System.nanoTime - t0) * 1e-6
      val response = Json.obj(
        "valid"          -> tracedDiff.resultE.isRight,
        "validationTime" -> timeSpent,
        "trace"          -> tracedDiff.trace.map(_.loggedJson.toString)
      )
      tracedDiff.resultE.fold(
        err => response + ("error" -> JsString(ApiError.fromValidationError(err).message)),
        _ => response
      )
    })

  def stateChanges: Route = stateChangesById ~ stateChangesByAddress

  def stateChangesById: Route = (get & path("stateChanges" / "info" / B58Segment)) { id =>
    blockchain.transactionInfo(id) match {
      case Some((h, tx: InvokeScriptTransaction)) =>
        val resultE = blockchain
          .invokeScriptResult(TransactionId(tx.id()))
          .map(isr => tx.json.map(_ ++ Json.obj("height" -> h, "stateChanges" -> isr))())
        complete(resultE)

      case Some((_, tx)) =>
        complete(ApiError.UnsupportedTransactionType)

      case None =>
        complete(ApiError.TransactionDoesNotExist)

    }
  }

  def stateChangesByAddress: Route =
    (get & path("stateChanges" / "address" / AddrSegment / "limit" / IntNumber) & parameter('after.?)) { (address, limit, afterOpt) =>
      (validate(limit <= settings.transactionsByAddressLimit, s"Max limit is ${settings.transactionsByAddressLimit}") & extractScheduler) {
        implicit sc =>
          import cats.implicits._

          val result = blockchain
            .addressTransactionsObservable(address, Set.empty, afterOpt.flatMap(str => Base58.tryDecodeWithLimit(str).map(ByteStr(_)).toOption))
            .map {
              case (height, tx: InvokeScriptTransaction) =>
                blockchain
                  .invokeScriptResult(TransactionId(tx.id()))
                  .map(isr => tx.json() ++ Json.obj("height" -> JsNumber(height), "stateChanges" -> isr))

              case (height, tx) =>
                Right(tx.json() ++ Json.obj("height" -> JsNumber(height)))
            }
            .take(limit)
            .toListL
            .map(_.sequence)

          complete(result.runAsyncLogErr)
      }
    }
}

object DebugApiRoute {
  implicit val assetsFormat: Format[Map[ByteStr, Long]] = Format[Map[ByteStr, Long]](
    {
      case JsObject(m) =>
        m.foldLeft[JsResult[Map[ByteStr, Long]]](JsSuccess(Map.empty)) {
          case (e: JsError, _) => e
          case (JsSuccess(m, _), (rawAssetId, JsNumber(count))) =>
            (ByteStr.decodeBase58(rawAssetId), count) match {
              case (Success(assetId), count) if count.isValidLong => JsSuccess(m.updated(assetId, count.toLong))
              case (Failure(_), _)                                => JsError(s"Can't parse '$rawAssetId' as base58 string")
              case (_, count)                                     => JsError(s"Invalid count of assets: $count")
            }
          case (_, (_, rawCount)) =>
            JsError(s"Invalid count of assets: $rawCount")
        }
      case _ => JsError("The map is expected")
    },
    m => Json.toJson(m.map { case (assetId, count) => assetId.base58 -> count })
  )
  implicit val leaseInfoFormat: Format[LeaseBalance] = Json.format

  case class AccountMiningInfo(address: String, miningBalance: Long, timestamp: Long)

  implicit val accountMiningBalanceFormat: Format[AccountMiningInfo] = Json.format

  implicit val addressWrites: Format[Address] = new Format[Address] {
    override def writes(o: Address): JsValue = JsString(o.stringRepr)

    override def reads(json: JsValue): JsResult[Address] = ???
  }

  case class HistoryInfo(lastBlockIds: Seq[BlockId], microBlockIds: Seq[BlockId])

  implicit val historyInfoFormat: Format[HistoryInfo] = Format(
    Reads { json =>
      ???
    },
    Writes { info =>
      ???
    }
  )

  implicit val hrCacheSizesFormat: Format[HistoryReplier.CacheSizes]          = Json.format
  implicit val mbsCacheSizesFormat: Format[MicroBlockSynchronizer.CacheSizes] = Json.format
  implicit val BigIntWrite: Writes[BigInt]                                    = (bigInt: BigInt) => JsNumber(BigDecimal(bigInt))
  implicit val scoreReporterStatsWrite: Writes[RxScoreObserver.Stats]         = Json.writes[RxScoreObserver.Stats]

  import MinerDebugInfo._
  implicit val minerStateWrites: Writes[MinerDebugInfo.State] = (s: MinerDebugInfo.State) =>
    JsString(s match {
      case MiningBlocks      => "mining blocks"
      case MiningMicroblocks => "mining microblocks"
      case Disabled          => "disabled"
      case Error(err)        => s"error: $err"
    })
}
