package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings

case class LeaseBroadcastApiRoute(settings: RestAPISettings, utxPoolSynchronizer: UtxPoolSynchronizer)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {
  override val route = pathPrefix("leasing" / "broadcast") {
    signedLease ~ signedLeaseCancel
  }

  def signedLease: Route = (path("lease") & post) {
    json[SignedLeaseV1Request] { leaseReq =>
      broadcastIfSuccess(leaseReq.toTx)
    }
  }

  def signedLeaseCancel: Route = (path("cancel") & post) {
    json[SignedLeaseCancelV1Request] { leaseCancelReq =>
      broadcastIfSuccess(leaseCancelReq.toTx)
    }
  }
}
