package scorex.waves.http

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json.{JsArray, Json}
import scorex.api.http._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.History
import scorex.wallet.Wallet

@Path("/debug")
@Api(value = "/debug")
case class DebugApiRoute(settings: RestAPISettings, wallet: Wallet, stateReader: StateReader, history: History) extends ApiRoute {

  override lazy val route = pathPrefix("debug") {
    blocks ~ state ~ info
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
}
