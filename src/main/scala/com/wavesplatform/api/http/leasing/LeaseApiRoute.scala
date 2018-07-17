package com.wavesplatform.api.http.leasing

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.JsNumber
import com.wavesplatform.account.Address
import com.wavesplatform.api.http._
import com.wavesplatform.api.http.leasing.LeaseCancelV1Request.leaseCancelRequestFormat
import com.wavesplatform.api.http.leasing.LeaseV1Request.leaseCancelRequestFormat
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.utils.Time
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease.{LeaseTransaction, LeaseTransactionV1}
import com.wavesplatform.wallet.Wallet

@Path("/leasing")
@Api(value = "/leasing")
case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utx: UtxPool, allChannels: ChannelGroup, time: Time)
    extends ApiRoute
    with BroadcastRoute {

  override val route = pathPrefix("leasing") {
    lease ~ cancel ~ active
  }

  @Path("/lease")
  @ApiOperation(value = "Creates a lease", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.api.http.leasing.LeaseV1Request",
        defaultValue =
          "{\n\t\"amount\": 100000000,\n\t\"recipient\": \"3NBsppTVpai9jq6agi9wXXrWhaMPPig48Aw\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000\n}"
      )
    ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def lease: Route = processRequest("lease", (t: LeaseV1Request) => doBroadcast(TransactionFactory.leaseV1(t, wallet, time)))

  @Path("/cancel")
  @ApiOperation(value = "Interrupt a lease", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.wavesplatform.api.http.leasing.LeaseCancelV1Request",
        defaultValue =
          "{\n\t\"sender\": \"3Myss6gmMckKYtka3cKCM563TBJofnxvfD7\",\n\t\"txId\": \"ABMZDPY4MyQz7kKNAevw5P9eNmRErMutJoV9UNeCtqRV\",\n\t\"fee\": 10000000\n}"
      )
    ))
  def cancel: Route = processRequest("cancel", (t: LeaseCancelV1Request) => doBroadcast(TransactionFactory.leaseCancelV1(t, wallet, time)))

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
          blockchain
            .addressTransactions(a, Set(LeaseTransactionV1.typeId), Int.MaxValue, 0)
            .collect {
              case (h, lt: LeaseTransaction) if blockchain.leaseDetails(lt.id()).exists(_.isActive) =>
                lt.json() + ("height" -> JsNumber(h))
            }
      })
    }
  }
}
