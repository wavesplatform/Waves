package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.common.{CommonAccountsApi, CommonTransactionsApi}
import com.wavesplatform.api.common.CommonAccountsApi.LeaseInfo
import com.wavesplatform.api.http.TransactionsApiRoute.TransactionJsonSerializer
import com.wavesplatform.api.http.leasing.LeaseApiRoute._
import com.wavesplatform.api.http.requests.{LeaseCancelRequest, LeaseRequest}
import com.wavesplatform.api.http.{BroadcastRoute, _}
import com.wavesplatform.utils.byteStrFormat
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.{JsNumber, Json, Writes}

case class LeaseApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    blockchain: Blockchain,
    transactionPublisher: TransactionPublisher,
    time: Time,
    commonAccountApi: CommonAccountsApi,
    transactionsApi: CommonTransactionsApi
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {

  private val serializer = TransactionJsonSerializer(blockchain, transactionsApi)

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
      val leaseInfoJson =
        if (blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls))
          commonAccountApi.activeLeases(address).map(Json.toJson(_))
        else
          commonAccountApi
            .activeLeasesOld(address)
            .collect {
              case (height, tx: LeaseTransaction) =>
                tx.json() ++ serializer.resolvedAliasTxFields(tx) + ("height" -> JsNumber(height))
            }

      complete(leaseInfoJson.toListL.runToFuture)
    }
  }
}

object LeaseApiRoute {
  implicit lazy val leaseInfoWrites: Writes[LeaseInfo] = Json.writes[LeaseInfo]
}
