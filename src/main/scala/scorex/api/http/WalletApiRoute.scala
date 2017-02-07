package scorex.api.http

import javax.ws.rs.Path
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.crypto.encode.Base58
import scorex.wallet.Wallet

@Path("/wallet")
@Api(value = "/wallet", description = "Wallet-related calls")
case class WalletApiRoute(settings: RestAPISettings, wallet: Wallet
) extends ApiRoute with CommonTransactionApiFunctions {

  override lazy val route = seed

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Export wallet seed", httpMethod = "GET")
  def seed: Route = {
    path("wallet" / "seed") {
      withAuth {
        getJsonRoute {
          JsonResponse(Json.obj("seed" -> Base58.encode(wallet.seed)), StatusCodes.OK)
        }
      }
    }
  }
}
