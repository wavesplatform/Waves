package scorex.waves.http

import java.net.{InetAddress, URI}
import javax.ws.rs.Path

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.network.PeerDatabase
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json.{JsArray, Json}
import scorex.api.http._
import scorex.block.Block
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.History
import scorex.wallet.Wallet

import scala.util.Success
import scala.util.control.NonFatal

@Path("/debug")
@Api(value = "/debug")
case class DebugApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    stateReader: StateReader,
    history: History,
    peerDatabase: PeerDatabase,
    rollbackToBlock: Block.BlockId => Unit) extends ApiRoute {

  override lazy val route = pathPrefix("debug") {
    blocks ~ state ~ info ~ stateWaves ~ rollback ~ rollbackTo ~ blacklist
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
        val bytes = block.bytes
        Json.obj(bytes.length.toString -> Base58.encode(FastCryptographicHash(bytes)))
      }))
    }
  }

  @Path("/state")
  @ApiOperation(value = "State", notes = "Get current state", httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json state")))
  def state: Route = (path("state") & get) {
    complete(stateReader.accountPortfolios
      .map { case (k, v) =>
        k.address -> v.balance
      }
    )
  }


  @Path("/stateWaves/{height}")
  @ApiOperation(value = "State at block", notes = "Get state at specified height", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "height", value = "height", required = true, dataType = "integer", paramType = "path")
  ))
  def stateWaves: Route = (path("stateWaves" / IntNumber) & get) { height =>
    val result = stateReader.accountPortfolios.keys
      .map(acc => acc.stringRepr -> stateReader.balanceAtHeight(acc, height))
      .filter(_._2 != 0)
      .toMap
    complete(result)
  }

  @Path("/rollback")
  @ApiOperation(value = "Rollback to height", notes = "Removes all blocks after given height", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "height", value = "Height to rollback to", required = true, dataType = "integer", paramType = "body")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "200 if success, 404 if there are no block at this height")
  ))
  def rollback: Route = withAuth {
    (path("rollback") & post) {
      entity(as[Int]) { rollbackTo =>
        history.blockAt(rollbackTo) match {
          case Some(block) =>
            rollbackToBlock(block.uniqueId)
            complete(StatusCodes.Accepted)
          case None =>
            complete(StatusCodes.BadRequest, "Block at height not found")
        }
      } ~ complete(StatusCodes.BadRequest)
    }
  }

  @Path("/info")
  @ApiOperation(value = "State", notes = "All info you need to debug", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def info: Route = (path("info") & get) {
    val stateHash = (BigInt(FastCryptographicHash(stateReader.accountPortfolios.toString().getBytes)) % Int.MaxValue).toInt
    complete(Json.obj(
      "stateHeight" -> stateReader.height,
      "stateHash" -> stateHash
    ))
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
          rollbackToBlock(sig)
          complete(StatusCodes.Accepted)
        case _ =>
          complete(InvalidSignature)
      }
    }
  }

  @Path("/blacklist")
  @ApiOperation(value = "Blacklits given peer", notes = "Moving peer to blacklist", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "IP:PORT address of node", required = true, dataType = "string", paramType = "body")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "200 if success, 404 if there are no peer with such address")
  ))
  def blacklist: Route = withAuth {
    (path("blacklist") & post) {
      entity(as[String]) { socketAddressString =>
        try {
          val uri = new URI("node://" + socketAddressString)
          peerDatabase.blacklistHost(InetAddress.getByName(uri.getHost))
          complete(StatusCodes.OK)
        } catch {
          case NonFatal(_) => complete(StatusCodes.BadRequest)
        }
      } ~ complete(StatusCodes.BadRequest)
    }
  }

}
