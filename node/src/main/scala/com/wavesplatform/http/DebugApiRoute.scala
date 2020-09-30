package com.wavesplatform.http

import java.net.{InetAddress, InetSocketAddress, URI}
import java.util.concurrent.ConcurrentMap

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import cats.implicits._
import cats.kernel.Monoid
import com.typesafe.config.{ConfigObject, ConfigRenderOptions}
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi, CommonTransactionsApi}
import com.wavesplatform.api.http.TransactionsApiRoute.applicationStatus
import com.wavesplatform.api.http._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{Miner, MinerDebugInfo}
import com.wavesplatform.network.{PeerDatabase, PeerInfo, _}
import com.wavesplatform.settings.{RestAPISettings, WavesSettings}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.{Blockchain, LeaseBalance, NG, Portfolio, StateHash}
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvalidRequestSignature}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

case class DebugApiRoute(
    ws: WavesSettings,
    time: Time,
    blockchain: Blockchain with NG,
    wallet: Wallet,
    accountsApi: CommonAccountsApi,
    transactionsApi: CommonTransactionsApi,
    assetsApi: CommonAssetsApi,
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
    loadBalanceHistory: Address => Seq[(Int, Long)],
    loadStateHash: Int => Option[StateHash]
) extends ApiRoute
    with AuthRoute
    with ScorexLogging {

  import DebugApiRoute._

  private lazy val configStr             = configRoot.render(ConfigRenderOptions.concise().setJson(true).setFormatted(true))
  private lazy val fullConfig: JsValue   = Json.parse(configStr)
  private lazy val wavesConfig: JsObject = Json.obj("waves" -> (fullConfig \ "waves").get)

  override val settings: RestAPISettings = ws.restAPISettings
  override lazy val route: Route = pathPrefix("debug") {
    stateChanges ~ balanceHistory ~ stateHash ~ validate ~ withAuth {
      state ~ info ~ stateWaves ~ rollback ~ rollbackTo ~ blacklist ~ portfolios ~ minerInfo ~ configInfo ~ print
    }
  }

  def print: Route =
    path("print")(jsonPost[DebugMessage] { params =>
      log.debug(params.message.take(250))
      ""
    })

  def portfolios: Route = path("portfolios" / AddrSegment) { address =>
    (get & parameter("considerUnspent".as[Boolean].?)) { considerUnspent =>
      extractScheduler { implicit s =>
        complete(accountsApi.portfolio(address).toListL.runToFuture.map { assetList =>
          val bd   = accountsApi.balanceDetails(address)
          val base = Portfolio(bd.regular, LeaseBalance(bd.leaseIn, bd.leaseOut), assetList.toMap)
          if (considerUnspent.getOrElse(true)) Monoid.combine(base, utxStorage.pessimisticPortfolio(address)) else base
        })
      }
    }
  }

  def balanceHistory: Route = (path("balances" / "history" / AddrSegment) & get) { address =>
    complete(Json.toJson(loadBalanceHistory(address).map {
      case (h, b) => Json.obj("height" -> h, "balance" -> b)
    }))
  }

  private def distribution(height: Int): Route = optionalHeaderValueByType[Accept](()) { accept =>
    extractScheduler { implicit s =>
      complete(
        assetsApi
          .wavesDistribution(height, None)
          .toListL
          .runToFuture
          .map {
            case l if accept.exists(_.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings)) =>
              Json.obj(l.map { case (address, balance) => address.toString -> (balance.toString: JsValueWrapper) }: _*)
            case l =>
              Json.obj(l.map { case (address, balance) => address.toString -> (balance: JsValueWrapper) }: _*)
          }
      )
    }
  }

  def state: Route = (path("state") & get) {
    distribution(blockchain.height)
  }

  def stateWaves: Route = (path("stateWaves" / IntNumber) & get) { height =>
    distribution(height)
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
      blockchain.blockHeader(params.rollbackTo) match {
        case Some(sh) =>
          rollbackToBlock(sh.id(), params.returnTransactionsToUtx)
        case None =>
          (StatusCodes.BadRequest, "Block at height not found")
      }
    } ~ complete(StatusCodes.BadRequest)
  }

  def info: Route = (path("info") & get) {
    complete(
      Json.obj(
        "stateHeight"                      -> blockchain.height,
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
        .filterNot(account => blockchain.hasAccountScript(account.toAddress))
        .map { account =>
          (account.toAddress, miner.getNextBlockGenerationOffset(account))
        }
        .collect {
          case (address, Right(offset)) =>
            AccountMiningInfo(
              address.stringRepr,
              blockchain.effectiveBalance(
                address,
                ws.blockchainSettings.functionalitySettings.generatingBalanceDepth(blockchain.height),
                blockchain.microblockIds.lastOption
              ),
              System.currentTimeMillis() + offset.toMillis
            )
        }
    )
  }

  def configInfo: Route = (path("configInfo") & get & parameter("full".as[Boolean])) { full =>
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
      val t0 = System.nanoTime
      val tracedDiff = for {
        tx <- TracedResult(TransactionFactory.fromSignedRequest(jsv))
        ei <- TransactionDiffer(blockchain.lastBlockTimestamp, time.correctedTime())(blockchain, tx)
      } yield (tx, ei)

      val timeSpent = (System.nanoTime - t0) * 1e-6
      val error = tracedDiff.resultE match {
        case Right((tx, diff)) => diff.errorMessage(tx.id()).map(em => GenericError(em.text))
        case Left(err)         => Some(err)
      }
      log.error(tracedDiff.resultE.toString)

      val response = Json.obj(
        "valid"          -> error.isEmpty,
        "validationTime" -> timeSpent.toLong,
        "trace"          -> tracedDiff.trace.map(_.loggedJson)
      )
      error.fold(response)(err => response + ("error" -> JsString(ApiError.fromValidationError(err).message)))
    })

  def stateChanges: Route = stateChangesById ~ stateChangesByAddress

  def stateChangesById: Route = (get & path("stateChanges" / "info" / TransactionId)) { id =>
    transactionsApi.transactionById(id) match {
      case Some((height, Right((ist, isr)), succeeded)) =>
        complete(ist.json() ++ applicationStatus(isBlockV5(height), succeeded) ++ Json.obj("height" -> height.toInt, "stateChanges" -> isr))
      case Some(_) => complete(ApiError.UnsupportedTransactionType)
      case None    => complete(ApiError.TransactionDoesNotExist)
    }
  }

  def stateChangesByAddress: Route =
    (get & path("stateChanges" / "address" / AddrSegment / "limit" / IntNumber) & parameter("after".as[ByteStr].?)) { (address, limit, afterOpt) =>
      validate(limit <= settings.transactionsByAddressLimit, s"Max limit is ${settings.transactionsByAddressLimit}") {
        extractScheduler { implicit s =>
          complete {
            implicit val ss: JsonEntityStreamingSupport = EntityStreamingSupport.json()

            Source
              .fromPublisher(
                transactionsApi
                  .invokeScriptResults(address, None, Set.empty, afterOpt)
                  .map {
                    case (height, Right((ist, isr)), succeeded) =>
                      ist.json() ++ applicationStatus(isBlockV5(height), succeeded) ++ Json.obj("height" -> JsNumber(height), "stateChanges" -> isr)
                    case (height, Left(tx), succeeded) =>
                      tx.json() ++ applicationStatus(isBlockV5(height), succeeded) ++ Json.obj("height" -> JsNumber(height))
                  }
                  .toReactivePublisher
              )
              .take(limit)
          }
        }
      }
    }

  def stateHash: Route =
    (get & path("stateHash" / IntNumber)) { height =>
      val result = for {
        sh      <- loadStateHash(height)
        h <- blockchain.blockHeader(height)
      } yield Json.toJson(sh).as[JsObject] ++ Json.obj("blockId" -> h.id().toString)

      result match {
        case Some(value) => complete(value)
        case None        => complete(StatusCodes.NotFound)
      }
    }

  private def isBlockV5(height: Int): Boolean = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, height)
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
    m => Json.toJson(m.map { case (assetId, count) => assetId.toString -> count })
  )
  implicit val leaseInfoFormat: Format[LeaseBalance] = Json.format

  case class AccountMiningInfo(address: String, miningBalance: Long, timestamp: Long)

  implicit val accountMiningBalanceFormat: Format[AccountMiningInfo] = Json.format

  implicit val addressWrites: Writes[Address] = Writes((a: Address) => JsString(a.stringRepr))

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
