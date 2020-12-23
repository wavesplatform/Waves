package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.common.CommonAccountsApi
import com.wavesplatform.api.http.requests.{LeaseCancelRequest, LeaseRequest}
import com.wavesplatform.api.http.{BroadcastRoute, _}
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.JsNumber

case class LeaseApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    blockchain: Blockchain,
    transactionPublisher: TransactionPublisher,
    time: Time,
    commonAccountApi: CommonAccountsApi
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

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
    path(AddrSegment) { address =>
      complete(
        commonAccountApi
          .activeLeases(address)
          .collect {
            case (height, leaseTransaction: LeaseTransaction) =>
              leaseTransaction.json() + ("height" -> JsNumber(height))
          }
          .toListL
          .runToFuture
      )
    }
  }
}
