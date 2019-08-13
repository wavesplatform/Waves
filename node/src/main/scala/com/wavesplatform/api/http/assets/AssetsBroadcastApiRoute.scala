package com.wavesplatform.api.http.assets

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.transaction.TxValidationError._

import scala.util.Left

case class AssetsBroadcastApiRoute(settings: RestAPISettings, utxPoolSynchronizer: UtxPoolSynchronizer)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {

  override val route: Route = pathPrefix("assets" / "broadcast") {
    issue ~ reissue ~ transfer ~ burnRoute ~ exchange
  }

  def issue: Route = broadcast[SignedIssueV1Request]("issue", _.toTx)

  def reissue: Route = broadcast[SignedReissueV1Request]("reissue", _.toTx)

  def burnRoute: Route = broadcast[SignedBurnV1Request]("burn", _.toTx)

  def transfer: Route =
    broadcast[SignedTransferRequests]("transfer", { transferReq =>
      transferReq.eliminate(
        _.toTx,
        _.eliminate(
          _.toTx,
          _ => Left(UnsupportedTransactionType)
        )
      )
    })

  def exchange: Route = broadcast[SignedExchangeRequest]("exchange", _.toTx)
}
