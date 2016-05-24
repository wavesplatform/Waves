package scorex.waves.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http._
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.LagonakiTransaction
import scorex.transaction.state.database.blockchain.PersistentLagonakiState

@Path("/debug")
@Api(value = "/debug", description = "Debug methods", position = 1)
case class DebugApiRoute(override val application: Application[LagonakiTransaction])(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  implicit lazy val transactionModule = application.transactionModule
  lazy val wallet = application.wallet

  override lazy val route = pathPrefix("debug") {
    blocks ~ state ~ stateAt ~ info ~ settings
  }

  @Path("/blocks/{howMany}")
  @ApiOperation(value = "Blocks", notes = "Get sizes and full hashes for last blocks", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "howMany",
      value = "How many last blocks to take",
      required = true,
      dataType = "String",
      paramType = "path")
  ))
  def blocks: Route = {
    path("blocks" / IntNumber) { case howMany =>
      getJsonRoute {
        Json.arr(application.blockStorage.history.lastBlocks(howMany).map { block =>
          val bytes = block.bytes
          Json.obj(bytes.length.toString -> Base58.encode(FastCryptographicHash(bytes)))
        })
      }
    }
  }

  @Path("/state")
  @ApiOperation(value = "State", notes = "Get current state", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def state: Route = {
    path("state") {
      getJsonRoute {
        application.blockStorage.state.asInstanceOf[PersistentLagonakiState].toJson(None)
      }
    }
  }

  @Path("/state/{height}")
  @ApiOperation(value = "State at block", notes = "Get state at specified height", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "height", value = "height", required = true, dataType = "Int", paramType = "path")
  ))
  def stateAt: Route = {
    path("state" / IntNumber) { case height =>
      getJsonRoute {
        application.blockStorage.state.asInstanceOf[PersistentLagonakiState].toJson(Some(height))
      }
    }
  }

  @Path("/info")
  @ApiOperation(value = "State", notes = "All info you need to debug", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def info: Route = {
    path("info") {
      getJsonRoute {
        val state = application.blockStorage.state.asInstanceOf[PersistentLagonakiState]
        Json.obj(
          "stateHeight" -> state.stateHeight,
          "stateHash" -> state.hash
        )
      }
    }
  }

  @Path("/settings")
  @ApiOperation(value = "State", notes = "Settings file", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def settings: Route = {
    path("settings") {
      getJsonRoute {
        application.settings.settingsJSON
      }
    }
  }

}
