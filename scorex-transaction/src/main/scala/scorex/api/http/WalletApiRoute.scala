package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.crypto.Base58
import scorex.transaction.state.wallet.Wallet


@Api(value = "/wallet", description = "Wallet-related calls")
case class WalletApiRoute(wallet: Wallet)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  implicit val w = wallet

  override lazy val route = {
    pathPrefix("wallet") {
      root ~ seed
    }
  }

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Export wallet seed", httpMethod = "GET")
  def seed = {
    path("seed") {
      jsonRoute {
        lazy val seedJs = Json.obj("seed" -> Base58.encode(wallet.exportSeed()))
        walletNotExists().getOrElse(seedJs).toString
      }

    }
  }

  @Path("/")
  @ApiOperation(value = "Wallet", notes = "Display whether wallet exists or not", httpMethod = "GET")
  def root = {
    path("") {
      jsonRoute {
        Json.obj("exists" -> wallet.exists()).toString
      }
    }
  }
}
