package scorex.api.http.leasing

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import scorex.api.http._
import scorex.api.http.leasing.LeaseCancelRequest.leaseCancelRequestFormat
import scorex.api.http.leasing.LeaseRequest.leaseCancelRequestFormat
import scorex.transaction._
import scorex.transaction.state.database.blockchain.StoredState
import scorex.wallet.Wallet

@Path("/leasing")
@Api(value = "/leasing/")
case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, state: StoredState, transactionModule: TransactionOperations)
  extends ApiRoute {

  override val route = pathPrefix("leasing") {
    lease ~ cancel
  }

  @Path("/lease")
  @ApiOperation(value = "Creates a lease",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.leasing.LeaseRequest",
      defaultValue = "{\n\t\"amount\": 100000000,\n\t\"recipient\": \"3NBsppTVpai9jq6agi9wXXrWhaMPPig48Aw\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def lease: Route = processRequest("lease", (t: LeaseRequest) => transactionModule.lease(t, wallet))

  @Path("/cancel")
  @ApiOperation(value = "Interrupt a lease",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.leasing.LeaseCancelRequest",
      defaultValue = "{\n\t\"sender\": \"3Myss6gmMckKYtka3cKCM563TBJofnxvfD7\",\n\t\"txId\": \"ABMZDPY4MyQz7kKNAevw5P9eNmRErMutJoV9UNeCtqRV\",\n\t\"fee\": 10000000\n}"
    )
  ))
  def cancel: Route = processRequest("cancel", (t: LeaseCancelRequest) => transactionModule.leaseCancel(t, wallet))
}
