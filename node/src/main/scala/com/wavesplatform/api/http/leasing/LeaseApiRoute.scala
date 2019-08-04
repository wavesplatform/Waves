package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonAccountApi
import com.wavesplatform.api.http._
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.JsNumber

@Path("/leasing")
@Api(value = "/leasing")
case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utxPoolSynchronizer: UtxPoolSynchronizer, time: Time)
    extends ApiRoute
    with BroadcastRoute
    with WithSettings {

  private[this] val commonAccountApi = new CommonAccountApi(blockchain)

  override val route = pathPrefix("leasing") {
    lease ~ cancel ~ active
  }

  def lease: Route = processRequest("lease", (t: LeaseV1Request) => broadcastIfSuccess(TransactionFactory.leaseV1(t, wallet, time)))

  def cancel: Route = processRequest("cancel", (t: LeaseCancelV1Request) => broadcastIfSuccess(TransactionFactory.leaseCancelV1(t, wallet, time)))

  @Path("/active/{address}")
  @ApiOperation(value = "Get all active leases for an address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path")
    ))
  def active: Route = (pathPrefix("active") & get) {
    pathPrefix(Segment) { address =>
      complete(Address.fromString(address) match {
        case Left(e) => ApiError.fromValidationError(e)
        case Right(a) =>
          commonAccountApi.activeLeases(a).map(_.collect {
            case (height, leaseTransaction: LeaseTransaction) =>
              leaseTransaction.json() + ("height" -> JsNumber(height))
          })
      })
    }
  }
}
