package scorex.waves.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.api.http._
import scorex.app.RunnableApplication
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.state.database.blockchain.StoredState

@Path("/debug")
@Api(value = "/debug")
case class DebugApiRoute(application: RunnableApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  val settings = application.settings
  implicit lazy val transactionModule = application.transactionModule
  lazy val wallet = application.wallet

  override lazy val route = pathPrefix("debug") {
    blocks ~ state ~ stateAt ~ info ~ getSettings
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
        val json = Json.arr(application.blockStorage.history.lastBlocks(howMany).map { block =>
          val bytes = block.bytes
          Json.obj(bytes.length.toString -> Base58.encode(FastCryptographicHash(bytes)))
        })

        JsonResponse(json, StatusCodes.OK)
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
        JsonResponse(application.blockStorage.state.asInstanceOf[StoredState].toJson(None), StatusCodes.OK)
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
        JsonResponse(application.blockStorage.state.asInstanceOf[StoredState].toJson(Some(height)), StatusCodes.OK)
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
        val state = application.blockStorage.state.asInstanceOf[StoredState]
        val json = Json.obj(
          "stateHeight" -> state.stateHeight,
          "stateHash" -> state.hash
        )

        JsonResponse(json, StatusCodes.OK)
      }
    }
  }

  @Path("/settings")
  @ApiOperation(value = "State", notes = "Settings file", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json state")
  ))
  def getSettings: Route = {
    path("settings") {
      withAuth {
        getJsonRoute {
          val json = Json.obj()
          //application.settings.settingsJSON

          JsonResponse(json, StatusCodes.OK)
        }
      }
    }
  }

}
