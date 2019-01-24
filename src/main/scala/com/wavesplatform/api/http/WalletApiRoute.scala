package com.wavesplatform.api.http

import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json.Json
import com.wavesplatform.wallet.Wallet

@Path("/wallet")
@Api(value = "/wallet")
case class WalletApiRoute(settings: RestAPISettings, wallet: Wallet) extends ApiRoute {

  override lazy val route = seed

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Export wallet seed", httpMethod = "GET")
  def seed: Route = (path("wallet" / "seed") & get & withAuth) {
    complete(Json.obj("seed" -> Base58.encode(wallet.seed)))
  }
}
