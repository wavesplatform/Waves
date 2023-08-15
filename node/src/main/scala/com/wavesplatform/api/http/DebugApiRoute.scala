package com.wavesplatform.api.http

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import cats.syntax.either.*
import com.typesafe.config.{ConfigObject, ConfigRenderOptions}
import com.wavesplatform.Version
import com.wavesplatform.account.{Address, PKKeyPair}
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi, CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{Miner, MinerDebugInfo}
import com.wavesplatform.network.{PeerDatabase, PeerInfo, *}
import com.wavesplatform.settings.{RestAPISettings, WavesSettings}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{Blockchain, Height, LeaseBalance, NG, Portfolio, StateHash, TxMeta}
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{GenericError, InvalidRequestSignature}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.script.trace.{InvokeScriptTrace, TracedResult}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.Channel
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import play.api.libs.json.*
import play.api.libs.json.Json.JsValueWrapper

import java.net.{InetAddress, InetSocketAddress, URI}
import java.util.concurrent.ConcurrentMap
import scala.concurrent.duration.*
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

case class DebugApiRoute(
    ws: WavesSettings,
    time: Time,
    blockchain: Blockchain & NG,
    wallet: Wallet,
    accountsApi: CommonAccountsApi,
    transactionsApi: CommonTransactionsApi,
    assetsApi: CommonAssetsApi,
    peerDatabase: PeerDatabase,
    establishedConnections: ConcurrentMap[Channel, PeerInfo],
    rollbackTask: (ByteStr, Boolean) => Task[Either[ValidationError, Unit]],
    utxStorage: UtxPool,
    miner: Miner & MinerDebugInfo,
    historyReplier: HistoryReplier,
    extLoaderStateReporter: Coeval[RxExtensionLoader.State],
    mbsCacheSizesReporter: Coeval[MicroBlockSynchronizer.CacheSizes],
    scoreReporter: Coeval[RxScoreObserver.Stats],
    configRoot: ConfigObject,
    loadBalanceHistory: Address => Seq[(Int, Long)],
    loadStateHash: Int => Option[StateHash],
    priorityPoolBlockchain: () => Blockchain,
    routeTimeout: RouteTimeout,
    heavyRequestScheduler: Scheduler
) extends ApiRoute
    with AuthRoute
    with ScorexLogging {

  import DebugApiRoute.*

  private lazy val configStr             = configRoot.render(ConfigRenderOptions.concise().setJson(true).setFormatted(true))
  private lazy val fullConfig: JsValue   = Json.parse(configStr)
  private lazy val wavesConfig: JsObject = Json.obj("waves" -> (fullConfig \ "waves").get)

  override val settings: RestAPISettings = ws.restAPISettings

  private[this] val serializer                                               = TransactionJsonSerializer(blockchain, transactionsApi)
  private[this] implicit val transactionMetaWrites: OWrites[TransactionMeta] = OWrites[TransactionMeta](serializer.transactionWithMetaJson)

  override lazy val route: Route = pathPrefix("debug") {
    stateChanges ~ balanceHistory ~ stateHash ~ validate ~ withAuth {
      state ~ info ~ stateWaves ~ rollback ~ rollbackTo ~ blacklist ~ minerInfo ~ configInfo ~ print
    }
  }

  def print: Route =
    path("print")(jsonPost[DebugMessage] { params =>
      log.debug(params.message.take(250))
      ""
    })

  def balanceHistory: Route = (path("balances" / "history" / AddrSegment) & get) { address =>
    complete(Json.toJson(loadBalanceHistory(address).map { case (h, b) =>
      Json.obj("height" -> h, "balance" -> b)
    }))
  }

  private def distribution(height: Int): Route = optionalHeaderValueByType(Accept) { accept =>
    routeTimeout.executeToFuture {
      assetsApi
        .wavesDistribution(height, None)
        .toListL
        .map {
          case l if accept.exists(_.mediaRanges.exists(CustomJson.acceptsNumbersAsStrings)) =>
            Json.obj(l.map { case (address, balance) => address.toString -> (balance.toString: JsValueWrapper) }*)
          case l =>
            Json.obj(l.map { case (address, balance) => address.toString -> (balance: JsValueWrapper) }*)
        }
    }
  }

  def state: Route = (path("state") & get) {
    distribution(blockchain.height)
  }

  def stateWaves: Route = (path("stateWaves" / IntNumber) & get) { height =>
    distribution(height)
  }

  private def rollbackToBlock(blockId: ByteStr, returnTransactionsToUtx: Boolean): Future[Either[ValidationError, JsObject]] = {
    implicit val sc: Scheduler = heavyRequestScheduler
    rollbackTask(blockId, returnTransactionsToUtx)
      .map(_.map(_ => Json.obj("BlockId" -> blockId.toString)))
      .runAsyncLogErr
  }

  def rollback: Route = (path("rollback") & withRequestTimeout(15.minutes)) {
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
    complete {
      val accounts = if (ws.minerSettings.privateKeys.nonEmpty) {
        ws.minerSettings.privateKeys.map(PKKeyPair(_))
      } else {
        wallet.privateKeyAccounts
      }

      accounts
        .filterNot(account => blockchain.hasAccountScript(account.toAddress))
        .map { account =>
          (account.toAddress, miner.getNextBlockGenerationOffset(account))
        }
        .collect { case (address, Right(offset)) =>
          AccountMiningInfo(
            address.toString,
            blockchain.effectiveBalance(
              address,
              ws.blockchainSettings.functionalitySettings.generatingBalanceDepth(blockchain.height),
              blockchain.microblockIds.lastOption
            ),
            System.currentTimeMillis() + offset.toMillis
          )
        }

    }
  }

  def configInfo: Route = (path("configInfo") & get & parameter("full".as[Boolean])) { full =>
    complete(if (full) fullConfig else wavesConfig)
  }

  def rollbackTo: Route = path("rollback-to" / Segment) { signature =>
    delete {
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
      val blockchain = priorityPoolBlockchain()
      val startTime  = System.nanoTime()

      val parsedTransaction = TransactionFactory.fromSignedRequest(jsv)

      val tracedDiff = for {
        tx   <- TracedResult(parsedTransaction)
        diff <- TransactionDiffer.forceValidate(blockchain.lastBlockTimestamp, time.correctedTime(), enableExecutionLog = true)(blockchain, tx)
      } yield (tx, diff)

      val error = tracedDiff.resultE match {
        case Right((tx, diff)) => diff.errorMessage(tx.id()).map(em => GenericError(em.text))
        case Left(err)         => Some(err)
      }

      val transactionJson = parsedTransaction.fold(_ => jsv, _.json())

      val serializer = tracedDiff.resultE
        .fold(
          _ => this.serializer,
          { case (_, diff) =>
            val compositeBlockchain = CompositeBlockchain(blockchain, diff)
            this.serializer.copy(blockchain = compositeBlockchain)
          }
        )

      val extendedJson = tracedDiff.resultE
        .fold(
          _ => jsv,
          { case (tx, diff) =>
            val meta = tx match {
              case ist: InvokeScriptTransaction =>
                val result = diff.scriptResults.get(ist.id())
                TransactionMeta.Invoke(Height(blockchain.height), ist, TxMeta.Status.Succeeded, diff.scriptsComplexity, result)
              case tx => TransactionMeta.Default(Height(blockchain.height), tx, TxMeta.Status.Succeeded, diff.scriptsComplexity)
            }
            serializer.transactionWithMetaJson(meta)
          }
        )

      val response = Json.obj(
        "valid"          -> error.isEmpty,
        "validationTime" -> (System.nanoTime() - startTime).nanos.toMillis,
        "trace" -> tracedDiff.trace.map {
          case ist: InvokeScriptTrace => ist.maybeLoggedJson(logged = true)(serializer.invokeScriptResultWrites)
          case trace                  => trace.loggedJson
        },
        "height" -> blockchain.height
      )

      error.fold(response ++ extendedJson)(err =>
        response + ("error" -> JsString(ApiError.fromValidationError(err).message)) + ("transaction" -> transactionJson)
      )
    })

  def stateChanges: Route = stateChangesById ~ stateChangesByAddress

  def stateChangesById: Route = (get & path("stateChanges" / "info" / TransactionId)) { id =>
    redirect(s"/transactions/info/$id", StatusCodes.MovedPermanently)
  }

  def stateChangesByAddress: Route =
    (get & path("stateChanges" / "address" / AddrSegment / "limit" / IntNumber) & parameter("after".as[ByteStr].?)) { (address, limit, afterOpt) =>
      validate(limit <= settings.transactionsByAddressLimit, s"Max limit is ${settings.transactionsByAddressLimit}") {
        implicit val ss: JsonEntityStreamingSupport = EntityStreamingSupport.json()
        routeTimeout.executeStreamed {
          transactionsApi
            .transactionsByAddress(address, None, Set.empty, afterOpt)
            .take(limit)
            .toListL
        }(Json.toJsObject(_))
      }
    }

  def stateHash: Route = (get & pathPrefix("stateHash")) {
    path("last")(stateHashAt(blockchain.height - 1)) ~ path(IntNumber)(stateHashAt)
  }

  private def stateHashAt(height: Int): Route = {
    val result = for {
      sh <- loadStateHash(height)
      h  <- blockchain.blockHeader(height)
    } yield Json.toJson(sh).as[JsObject] ++ Json.obj(
      "blockId" -> h.id().toString,
      "height"  -> height,
      "version" -> Version.VersionString
    )

    result match {
      case Some(value) => complete(value)
      case None        => complete(StatusCodes.NotFound)
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
    m => Json.toJson(m.map { case (assetId, count) => assetId.toString -> count })
  )

  implicit val leaseInfoFormat: Format[LeaseBalance] = Json.format

  case class AccountMiningInfo(address: String, miningBalance: Long, timestamp: Long)

  implicit val accountMiningBalanceFormat: Format[AccountMiningInfo] = Json.format

  implicit val addressWrites: Writes[Address] = Writes((a: Address) => JsString(a.toString))

  implicit val hrCacheSizesFormat: Format[HistoryReplier.CacheSizes]          = Json.format
  implicit val mbsCacheSizesFormat: Format[MicroBlockSynchronizer.CacheSizes] = Json.format
  implicit val BigIntWrite: Writes[BigInt]                                    = (bigInt: BigInt) => JsNumber(BigDecimal(bigInt))
  implicit val scoreReporterStatsWrite: Writes[RxScoreObserver.Stats]         = Json.writes[RxScoreObserver.Stats]

  import MinerDebugInfo.*
  implicit val minerStateWrites: Writes[MinerDebugInfo.State] = (s: MinerDebugInfo.State) =>
    JsString(s match {
      case MiningBlocks      => "mining blocks"
      case MiningMicroblocks => "mining microblocks"
      case Disabled          => "disabled"
      case Error(err)        => s"error: $err"
    })

  implicit val assetMapWrites: Writes[Map[IssuedAsset, Long]] = Writes { m =>
    Json.toJson(m.map { case (asset, balance) =>
      asset.id.toString -> JsNumber(balance)
    })
  }

  implicit val portfolioJsonWrites: Writes[Portfolio] = Writes { pf =>
    JsObject(
      Map(
        "balance" -> JsNumber(pf.balance),
        "lease"   -> Json.toJson(pf.lease),
        "assets"  -> Json.toJson(pf.assets)
      )
    )
  }
}
