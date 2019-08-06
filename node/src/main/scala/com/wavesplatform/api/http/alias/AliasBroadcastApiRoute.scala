package com.wavesplatform.api.http.alias

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings

case class AliasBroadcastApiRoute(settings: RestAPISettings, utxPoolSynchronizer: UtxPoolSynchronizer)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {
  override val route = pathPrefix("alias" / "broadcast") {
    signedCreate
  }

  def signedCreate: Route = (path("create") & post) {
    json[SignedCreateAliasV1Request] { aliasReq =>
      broadcastIfSuccess(aliasReq.toTx)
    }
  }
}
