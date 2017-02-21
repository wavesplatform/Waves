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
      dataType = "scorex.transaction.state.wallet.LeaseRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"untilBlock\":\"blockId\",\n\t\"recipient\":\"recipientId\"\n}"
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
      dataType = "scorex.transaction.state.wallet.LeaseCancelRequest",
      defaultValue = "{\n\t\"sender\":\"senderId\",\n\t\"txId\":\"leaseTranscationId\"\n}"
    )
  ))
  def cancel: Route = processRequest("cancel", (t: LeaseCancelRequest) => transactionModule.leaseCancel(t, wallet))
}
