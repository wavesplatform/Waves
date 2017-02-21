package scorex.api.http.leasing

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.api.http._
import scorex.transaction._

@Path("/leasing/broadcast")
@Api(value = "leasing")
case class LeaseBroadcastApiRoute(settings: RestAPISettings, transactionModule: TransactionModule)
  extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("leasing" / "broadcast") {
    signedLease ~ signedLeaseCancel
  }

  @Path("/lease")
  @ApiOperation(value = "Broadcasts a signed lease transaction",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.SignedLeaseRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"untilBlock\":\"blockId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedLease: Route =  json[SignedLeaseRequest] { leaseReq =>
    doBroadcast(leaseReq.toTx)
  }

  @Path("/cancel")
  @ApiOperation(value = "Broadcasts a signed lease cancel transaction",
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
  def signedLeaseCancel: Route =  json[SignedLeaseCancelRequest] { leaseCancelReq =>
    doBroadcast(leaseCancelReq.toTx)
  }
}
