package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.common.{CommonAccountsApi, LeaseInfo}
import com.wavesplatform.api.http.{BroadcastRoute, _}
import com.wavesplatform.api.http.requests.{LeaseCancelRequest, LeaseRequest}
import com.wavesplatform.api.http.ApiError.{InvalidIds, TooBigArrayAllocation, TransactionDoesNotExist}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json._

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
  import LeaseApiRoute._

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
    } ~ pathPrefix("info")(leaseInfo)

  private[this] def active: Route = (pathPrefix("active") & get & extractScheduler) { implicit sc =>
    path(AddrSegment) { address =>
      complete(commonAccountApi.activeLeases(address).map(Json.toJson(_)).toListL.runToFuture)
    }
  }

  private[this] def leaseInfo: Route =
    (get & path(TransactionId)) { leaseId =>
      val result = commonAccountApi
        .leaseInfo(leaseId)
        .toRight(TransactionDoesNotExist)

      complete(result)
    } ~ anyParam("id") { ids =>
      if (ids.size > settings.transactionsByAddressLimit)
        complete(TooBigArrayAllocation(settings.transactionsByAddressLimit))
      else
        leasingInfosMap(ids) match {
          case Left(err) => complete(err)
          case Right(leaseInfoByIdMap) =>
            val results = ids.map(leaseInfoByIdMap).toVector
            complete(results)
        }
    }

  private[this] def leasingInfosMap(ids: Iterable[String]): Either[InvalidIds, Map[String, LeaseInfo]] = {
    val infos = ids.map(
      id =>
        (for {
          id <- Base58.tryDecodeWithLimit(id).toOption
          li <- commonAccountApi.leaseInfo(ByteStr(id))
        } yield li).toRight(id)
    )
    val failed = infos.flatMap(_.left.toOption)

    if (failed.isEmpty) {
      Right(infos.collect {
        case Right(li) => li.id.toString -> li
      }.toMap)
    } else {
      Left(InvalidIds(failed.toVector))
    }
  }
}

object LeaseApiRoute {
  implicit val leaseStatusWrites: Writes[LeaseInfo.Status] =
    Writes(s => JsString(s.toString.toLowerCase))

  implicit val config: Aux[Json.MacroOptions] = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)

  implicit val leaseInfoWrites: OWrites[LeaseInfo] = {
    import com.wavesplatform.utils.byteStrFormat
    Json.writes[LeaseInfo]
  }
}
