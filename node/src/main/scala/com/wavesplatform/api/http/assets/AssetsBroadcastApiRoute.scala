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

  def issue: Route = (path("issue") & post) {
    json[SignedIssueV1Request] { issueReq =>
      broadcastIfSuccess(issueReq.toTx)
    }
  }

  def reissue: Route = (path("reissue") & post) {
    json[SignedReissueV1Request] { reissueReq =>
      broadcastIfSuccess(reissueReq.toTx)
    }
  }

  def burnRoute: Route = (path("burn") & post) {
    json[SignedBurnV1Request] { burnReq =>
      broadcastIfSuccess(burnReq.toTx)
    }
  }

  def transfer: Route = (path("transfer") & post) {
    json[SignedTransferRequests] { transferReq =>
      broadcastIfSuccess(
        transferReq.eliminate(
          _.toTx,
          _.eliminate(
            _.toTx,
            _ => Left(UnsupportedTransactionType)
          )
        )
      )
    }
  }

  def exchange: Route = (path("exchange") & post) {
    json[SignedExchangeRequest] { req =>
      broadcastIfSuccess(req.toTx)
    }
  }
}
