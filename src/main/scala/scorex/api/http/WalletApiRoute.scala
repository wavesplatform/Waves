package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.app.Application
import scorex.crypto.encode.Base58

@Path("/wallet")
@Api(value = "/wallet", description = "Wallet-related calls")
case class WalletApiRoute(application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {
  val settings = application.settings

  private val wallet = application.wallet

  override lazy val route = root ~ seed

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

  @Path("/")
  @ApiOperation(value = "Wallet", notes = "Display whether wallet exists or not", httpMethod = "GET")
  def root: Route = {
    path("wallet") {
      getJsonRoute {
        JsonResponse(Json.obj("exists" -> wallet.exists()), StatusCodes.OK)
      }
    }
  }
}
