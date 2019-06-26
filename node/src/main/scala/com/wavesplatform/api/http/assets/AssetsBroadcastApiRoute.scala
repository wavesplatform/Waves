package com.wavesplatform.api.http.assets

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup

import scala.util.Left

case class AssetsBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {

  override val route: Route = pathPrefix("assets" / "broadcast") {
    issue ~ reissue ~ transfer ~ burnRoute ~ batchTransfer ~ exchange
  }

  def issue: Route = (path("issue") & post) {
    json[SignedIssueV1Request] { issueReq =>
      doBroadcast(issueReq.toTx)
    }
  }

  def reissue: Route = (path("reissue") & post) {
    json[SignedReissueV1Request] { reissueReq =>
      doBroadcast(reissueReq.toTx)
    }
  }

  def burnRoute: Route = (path("burn") & post) {
    json[SignedBurnV1Request] { burnReq =>
      doBroadcast(burnReq.toTx)
    }
  }

  def batchTransfer: Route = (path("batch-transfer") & post) {
    json[List[SignedTransferRequests]](
      _.map(_.eliminate(_.toTx, _.eliminate(_.toTx, _ => Left(UnsupportedTransactionType))))
        .map(doBroadcast(_).json)
    )
  }

  def transfer: Route = (path("transfer") & post) {
    json[SignedTransferRequests] { transferReq =>
      doBroadcast(
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
      doBroadcast(req.toTx)
    }
  }
}
