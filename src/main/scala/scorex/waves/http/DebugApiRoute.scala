package scorex.waves.http

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.BlockStorage
import scorex.transaction.state.database.blockchain.StoredState
import scorex.wallet.Wallet

@Path("/debug")
@Api(value = "/debug")
case class DebugApiRoute(settings: RestAPISettings, wallet: Wallet, blockStorage: BlockStorage) extends ApiRoute {

  override lazy val route = pathPrefix("debug") {
    blocks ~ state ~ stateAt ~ info ~ getSettings ~ stateWaves
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
      complete(Json.arr(blockStorage.history.lastBlocks(howMany).map { block =>
        val bytes = block.bytes
        Json.obj(bytes.length.toString -> Base58.encode(FastCryptographicHash(bytes)))
      }))
    }
  }

  @Path("/state")
  @ApiOperation(value = "State", notes = "Get current state", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def state: Route = (path("state") & get) {
    complete(blockStorage.state.asInstanceOf[StoredState].toJson(None))
  }

  @Path("/state/{height}")
  @ApiOperation(value = "State at block", notes = "Get state at specified height", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "height", value = "height", required = true, dataType = "integer", paramType = "path")
  ))
  def stateAt: Route = (path("state" / IntNumber) & get) { height =>
    complete(blockStorage.state.asInstanceOf[StoredState].toJson(Some(height)))
  }

  @Path("/stateWaves/{height}")
  @ApiOperation(value = "State at block", notes = "Get state at specified height", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "height", value = "height", required = true, dataType = "integer", paramType = "path")
  ))
  def stateWaves: Route = (path("stateWaves" / IntNumber) & get) { height =>
    complete(blockStorage.state.asInstanceOf[StoredState].toWavesJson(height))
  }

  @Path("/info")
  @ApiOperation(value = "State", notes = "All info you need to debug", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def info: Route = (path("info") & get) {
    val state = blockStorage.state.asInstanceOf[StoredState]
    complete(Json.obj(
      "stateHeight" -> state.stateHeight,
      "stateHash" -> state.hash
    ))
  }

  @Path("/settings")
  @ApiOperation(value = "State", notes = "Settings file", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def getSettings: Route = (path("settings") & get & withAuth) {
    complete(Json.obj())
  }
}
