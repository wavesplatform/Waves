package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.wallet.Wallet
import spray.routing.Route


@Api(value = "/wallet", description = "Wallet-related calls")
case class WalletApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  private val wallet = application.wallet

  override lazy val route = {
    pathPrefix("wallet") {
      root ~ seed
    }
  }

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Export wallet seed", httpMethod = "GET")
  def seed: Route = {
    path("seed") {
      jsonRoute {
        lazy val seedJs = Json.obj("seed" -> Base58.encode(wallet.seed))
        walletNotExists(wallet).getOrElse(seedJs).toString
      }

    }
  }

  @Path("/")
  @ApiOperation(value = "Wallet", notes = "Display whether wallet exists or not", httpMethod = "GET")
  def root: Route = {
    path("") {
      jsonRoute {
        Json.obj("exists" -> wallet.exists()).toString
      }
    }
  }
}
