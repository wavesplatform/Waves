package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonAccountApi
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.requests.{LeaseCancelRequest, LeaseRequest}
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.JsNumber

case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utxPoolSynchronizer: UtxPoolSynchronizer, time: Time)
    extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  private[this] val commonAccountApi = new CommonAccountApi(blockchain)

  override val route: Route = pathPrefix("leasing") {
    active ~ deprecatedRoute
  }

  private def deprecatedRoute: Route =
    (path("lease") & withAuth) {
      broadcast[LeaseRequest](TransactionFactory.lease(_, wallet, time))
    } ~ (path("cancel") & withAuth) {
      broadcast[LeaseCancelRequest](TransactionFactory.leaseCancel(_, wallet, time))
    } ~ pathPrefix("broadcast") {
      path("lease")(broadcast[LeaseRequest](_.toTx)) ~
        path("cancel")(broadcast[LeaseCancelRequest](_.toTx))
    }

  def active: Route = (pathPrefix("active") & get & extractScheduler) { implicit sc =>
    pathPrefix(Segment) { address =>
      complete(Address.fromString(address) match {
        case Left(e) => ApiError.fromValidationError(e)
        case Right(a) =>
          commonAccountApi
            .activeLeases(a)
            .collect {
              case (height, leaseTransaction: LeaseTransaction) =>
                leaseTransaction.json() + ("height" -> JsNumber(height))
            }
            .toListL
            .runToFuture
      })
    }
  }
}
