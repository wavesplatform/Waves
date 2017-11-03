package scorex.waves.http

import java.net.{InetAddress, InetSocketAddress, URI}
import java.util.concurrent.ConcurrentMap
import javax.ws.rs.Path

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.network.{LocalScoreChanged, PeerDatabase, PeerInfo, _}
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.{ByteStr, LeaseInfo, Portfolio, StateReader}
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.Address
import scorex.api.http._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction._
import scorex.wallet.Wallet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.util.control.NonFatal
import DebugApiRoute._
import com.typesafe.config.{ConfigObject, ConfigRenderOptions}
import com.wavesplatform.mining.Miner
import com.wavesplatform.mining.MinerDebugInfo
import scorex.block.Block.BlockId
import scorex.utils.ScorexLogging


@Path("/debug")
@Api(value = "/debug")
case class DebugApiRoute(settings: RestAPISettings,
                         wallet: Wallet,
                         stateReader: StateReader,
                         history: History with DebugNgHistory,
                         peerDatabase: PeerDatabase,
                         establishedConnections: ConcurrentMap[Channel, PeerInfo],
                         blockchainUpdater: BlockchainUpdater,
                         allChannels: ChannelGroup,
                         utxStorage: UtxPool,
                         blockchainDebugInfo: BlockchainDebugInfo,
                         miner: Miner with MinerDebugInfo,
                         configRoot: ConfigObject
                        ) extends ApiRoute with ScorexLogging {

  private lazy val configStr = configRoot.render(ConfigRenderOptions.concise().setJson(true).setFormatted(true))
  private lazy val fullConfig: JsValue = Json.parse(configStr)
  private lazy val wavesConfig: JsObject = Json.obj("waves" -> (fullConfig \ "waves").get)

  override lazy val route: Route = pathPrefix("debug") {
    blocks ~ state ~ info ~ stateWaves ~ rollback ~ rollbackTo ~ blacklist ~ portfolios ~ minerInfo ~ historyInfo ~ configInfo ~ print
  }

  @Path("/blocks/{howMany}")
  @ApiOperation(value = "Blocks", notes = "Get sizes and full hashes for last blocks", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "howMany",
      value = "How many last blocks to take",
      required = true,
      dataType = "string",
      paramType = "path")
  ))
  def blocks: Route = {
    (path("blocks" / IntNumber) & get) { howMany =>
      complete(JsArray(history.lastBlocks(howMany).map { block =>
        val bytes = block.bytes()
        Json.obj(bytes.length.toString -> Base58.encode(FastCryptographicHash(bytes)))
      }))
    }
  }
  @Path("/print")
  @ApiOperation(
    value = "Print",
    notes = "Prints a string at DEBUG level, strips to 100 chars",
    httpMethod = "POST"
  )
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.waves.http.DebugMessage",
      defaultValue = "{\n\t\"message\": \"foo\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json portfolio")))
  def print: Route = (path("print") & post & withAuth) {
    json[DebugMessage] { params =>
      log.debug(params.message.take(100))
      ""
    }
  }


  @Path("/state")
  @ApiOperation(value = "State", notes = "Get current state", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json state")))
  def state: Route = (path("state") & get) {
    complete(stateReader().accountPortfolios
      .map { case (k, v) =>
        k.address -> v.balance
      }
    )
  }

  @Path("/portfolios/{address}")
  @ApiOperation(
    value = "Portfolio",
    notes = "Get current portfolio considering pessimistic transactions in the UTX pool",
    httpMethod = "GET"
  )
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "address",
      value = "An address of portfolio",
      required = true,
      dataType = "string",
      paramType = "path"
    ),
    new ApiImplicitParam(
      name = "considerUnspent",
      value = "Taking into account pessimistic transactions from UTX pool",
      required = false,
      dataType = "boolean",
      paramType = "query",
      defaultValue = "true"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json portfolio")))
  def portfolios: Route = path("portfolios" / Segment) { (rawAddress) =>
    (get & parameter('considerUnspent.as[Boolean])) { (considerUnspent) =>
      Address.fromString(rawAddress) match {
        case Left(_) => complete(InvalidAddress)
        case Right(address) =>
          val portfolio = if (considerUnspent) utxStorage.portfolio(address) else stateReader().accountPortfolio(address)
          complete(Json.toJson(portfolio))
      }
    }
  }

  @Path("/stateWaves/{height}")
  @ApiOperation(value = "State at block", notes = "Get state at specified height", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "height", value = "height", required = true, dataType = "integer", paramType = "path")
  ))
  def stateWaves: Route = (path("stateWaves" / IntNumber) & get) { height =>
    val s = stateReader()
    val result = s.accountPortfolios.keys
      .map(acc => acc.stringRepr -> s.balanceAtHeight(acc, height))
      .filter(_._2 != 0)
      .toMap
    complete(result)
  }

  private def rollbackToBlock(blockId: ByteStr, returnTransactionsToUtx: Boolean): Future[ToResponseMarshallable] = Future {
    blockchainUpdater.removeAfter(blockId) match {
      case Right(blocks) =>
        allChannels.broadcast(LocalScoreChanged(history.score(), LocalScoreChanged.Reason.Rollback))
        if (returnTransactionsToUtx) {
          blocks.flatMap(_.transactionData).foreach(tx => utxStorage.putIfNew(tx))
        }
        miner.scheduleMining()
        Json.obj("BlockId" -> blockId.toString): ToResponseMarshallable
      case Left(error) => ApiError.fromValidationError(error)
    }
  }

  @Path("/rollback")
  @ApiOperation(value = "Rollback to height", notes = "Removes all blocks after given height", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.waves.http.RollbackParams",
      defaultValue = "{\n\t\"rollbackTo\": 3,\n\t\"returnTransactionsToUTX\": false\n}"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "200 if success, 404 if there are no block at this height")
  ))
  def rollback: Route = (path("rollback") & post & withAuth) {
    json[RollbackParams] { params =>
      history.blockAt(params.rollbackTo) match {
        case Some(block) =>
          rollbackToBlock(block.uniqueId, params.returnTransactionsToUtx)
        case None =>
          (StatusCodes.BadRequest, "Block at height not found")
      }
    } ~ complete(StatusCodes.BadRequest)
  }

  @Path("/info")
  @ApiOperation(value = "State", notes = "All info you need to debug", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def info: Route = (path("info") & get) {
    complete(Json.obj(
      "stateHeight" -> stateReader().height,
      "stateHash" -> blockchainDebugInfo.persistedAccountPortfoliosHash,
      "blockchainDebugInfo" -> blockchainDebugInfo.debugInfo()
    ))
  }

  @Path("/minerInfo")
  @ApiOperation(value = "State", notes = "All miner info you need to debug", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def minerInfo: Route = (path("minerInfo") & get & withAuth) {
    complete(miner.collectNextBlockGenerationTimes.map { case (a, t) =>
      val s = stateReader()
      AccountMiningInfo(a.stringRepr,
        s.effectiveBalanceAtHeightWithConfirmations(a, s.height, 1000).get,
        t)
    })
  }


  @Path("/historyInfo")
  @ApiOperation(value = "State", notes = "All history info you need to debug", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def historyInfo: Route = (path("historyInfo") & get & withAuth) {
    val a = history.lastPersistedBlockIds(10)
    val b = history.microblockIds()
    complete(HistoryInfo(a, b))

  }


  @Path("/configInfo")
  @ApiOperation(value = "Config", notes = "Currently running node config", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "full",
      value = "Exposes full typesafe config",
      required = false,
      dataType = "boolean",
      paramType = "query",
      defaultValue = "false"
    )))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def configInfo: Route = (path("configInfo") & get & parameter('full.as[Boolean]) & withAuth) { full =>
    complete(if (full) fullConfig else wavesConfig)
  }

  @Path("/rollback-to/{signature}")
  @ApiOperation(value = "Block signature", notes = "Rollback the state to the block with a given signature", httpMethod = "DELETE")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "signature", value = "Base58-encoded block signature", required = true, dataType = "string", paramType = "path")
  ))
  def rollbackTo: Route = path("rollback-to" / Segment) { signature =>
    (delete & withAuth) {
      ByteStr.decodeBase58(signature) match {
        case Success(sig) =>
          complete(rollbackToBlock(sig, returnTransactionsToUtx = false))
        case _ =>
          complete(InvalidSignature)
      }
    }
  }

  @Path("/blacklist")
  @ApiOperation(value = "Blacklist given peer", notes = "Moving peer to blacklist", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "IP address of node", required = true, dataType = "string", paramType = "body")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "200 if success, 404 if there are no peer with such address")
  ))
  def blacklist: Route = (path("blacklist") & post & withAuth) {
    entity(as[String]) { socketAddressString =>
      try {
        val uri = new URI("node://" + socketAddressString)
        val address = InetAddress.getByName(uri.getHost)
        establishedConnections.entrySet().stream().forEach { entry =>
          entry.getValue.remoteAddress match {
            case x: InetSocketAddress if x.getAddress == address =>
              peerDatabase.blacklistAndClose(entry.getKey, "Debug API request")
            case _ =>
          }
        }
        complete(StatusCodes.OK)
      } catch {
        case NonFatal(_) => complete(StatusCodes.BadRequest)
      }
    } ~ complete(StatusCodes.BadRequest)
  }

}

object DebugApiRoute {
  implicit val assetsFormat: Format[Map[ByteStr, Long]] = Format[Map[ByteStr, Long]](
    {
      case JsObject(m) => m.foldLeft[JsResult[Map[ByteStr, Long]]](JsSuccess(Map.empty)) {
        case (e: JsError, _) => e
        case (JsSuccess(m, _), (rawAssetId, JsNumber(count))) =>
          (ByteStr.decodeBase58(rawAssetId), count) match {
            case (Success(assetId), count) if count.isValidLong => JsSuccess(m.updated(assetId, count.toLong))
            case (Failure(_), _) => JsError(s"Can't parse '$rawAssetId' as base58 string")
            case (_, count) => JsError(s"Invalid count of assets: $count")
          }
        case (_, (_, rawCount)) =>
          JsError(s"Invalid count of assets: $rawCount")
      }
      case _ => JsError("The map is expected")
    },
    m => Json.toJson(m.map { case (assetId, count) => assetId.base58 -> count })
  )
  implicit val leaseInfoFormat: Format[LeaseInfo] = Json.format
  implicit val portfolioFormat: Format[Portfolio] = Json.format

  case class AccountMiningInfo(address: String, miningBalance: Long, timestamp: Long)

  implicit val accountMiningBalanceFormat: Format[AccountMiningInfo] = Json.format

  implicit val hashInfoFormat: Format[HashInfo] = Json.format
  implicit val stateDebugInfoFormat: Format[StateDebugInfo] = Json.format

  implicit val addressWrites: Format[Address] = new Format[Address] {
    override def writes(o: Address): JsValue = JsString(o.stringRepr)

    override def reads(json: JsValue): JsResult[Address] = ???
  }

  implicit val byteStrWrites: Format[ByteStr] = new Format[ByteStr] {
    override def writes(o: AssetId): JsValue = JsString(o.base58)

    override def reads(json: JsValue): JsResult[BlockId] = ???
  }

  case class HistoryInfo(lastBlockIds: Seq[BlockId], microBlockIds: Seq[BlockId])

  implicit val historyInfoFormat: Format[HistoryInfo] = Json.format


}
