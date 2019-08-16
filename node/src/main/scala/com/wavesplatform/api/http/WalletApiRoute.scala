package com.wavesplatform.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.Json

@Path("/wallet")
@Api(value = "/wallet")
case class WalletApiRoute(settings: RestAPISettings, wallet: Wallet) extends ApiRoute with AuthRoute {

  override lazy val route = seed

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Export wallet seed", httpMethod = "GET")
  def seed: Route = (path("wallet" / "seed") & get & withAuth) {
    complete(Json.obj("seed" -> Base58.encode(wallet.seed)))
  }
}
