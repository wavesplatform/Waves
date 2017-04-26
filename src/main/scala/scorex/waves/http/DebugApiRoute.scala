package scorex.waves.http

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.reader.StateReader
import io.swagger.annotations._
import play.api.libs.json.{JsArray, Json}
import scorex.account.Account
import scorex.api.http._
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash
import scorex.transaction.History
import scorex.wallet.Wallet

@Path("/debug")
@Api(value = "/debug")
case class DebugApiRoute(settings: RestAPISettings, wallet: Wallet, stateReader: StateReader, history: History) extends ApiRoute {

  override lazy val route = pathPrefix("debug") {
    blocks ~ state ~ info ~ stateWaves
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
