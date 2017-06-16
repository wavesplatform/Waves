package scorex.waves.http

import java.net.{InetSocketAddress, URI}
import javax.ws.rs.Path

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json.{JsArray, Json}
import scorex.api.http._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.peer.PeerManager.AddToBlacklist
import scorex.transaction.{BlockchainUpdater, History}
import scorex.wallet.Wallet

import scala.util.Try

@Path("/debug")
@Api(value = "/debug")
case class DebugApiRoute(settings: RestAPISettings, wallet: Wallet, stateReader: StateReader,
                         blockchainUpdater: BlockchainUpdater, history: History, peerManager: ActorRef) extends ApiRoute {

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
            blockchainUpdater.removeAfter(block.uniqueId)
            complete(StatusCodes.OK)
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
      val maybeBlockId = ByteStr.decodeBase58(signature).toOption
      if (maybeBlockId.isEmpty) {
        complete(InvalidSignature)
      } else {
        if (blockchainUpdater.removeAfter(maybeBlockId.get)) complete(StatusCodes.OK) else complete(StatusCodes.BadRequest)
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
        val address = for {
          u <- Try(new URI("node://" + socketAddressString)).toEither
        } yield new InetSocketAddress(u.getHost, u.getPort)
        if (address.isRight) {
          peerManager ! AddToBlacklist(address.right.get)
          complete(StatusCodes.OK)
        } else complete(StatusCodes.BadRequest)
      } ~ complete(StatusCodes.BadRequest)
    }
  }

}
