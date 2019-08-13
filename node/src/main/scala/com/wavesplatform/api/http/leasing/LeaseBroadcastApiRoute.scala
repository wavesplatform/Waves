package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings

case class LeaseBroadcastApiRoute(settings: RestAPISettings, utxPoolSynchronizer: UtxPoolSynchronizer) extends BroadcastRoute with WithSettings {
  override val route = pathPrefix("leasing" / "broadcast") {
    signedLease ~ signedLeaseCancel
  }

  def signedLease: Route = broadcast[SignedLeaseV1Request]("lease", _.toTx)

  def signedLeaseCancel: Route = broadcast[SignedLeaseCancelV1Request]("cancel", _.toTx)
}
