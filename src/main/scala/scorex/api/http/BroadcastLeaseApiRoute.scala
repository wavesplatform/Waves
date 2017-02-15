package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import scorex.api.http.leasing.{SignedLeaseCancelRequest, SignedLeaseRequest}
import scorex.app.Application
import scorex.transaction.SimpleTransactionModule

@Path("/broadcast")
@Api(value = "/broadcast/")
case class BroadcastLeaseApiRoute(application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  val settings = application.settings

  private val wallet = application.wallet
  implicit val transactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]

  override val route =
    pathPrefix("leasing") {
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
      dataType = "scorex.transaction.state.wallet.SignedLeaseRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"untilBlock\":\"blockId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def lease: Route = path("lease") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          mkResponse(for {
            js <- parseToEither(body)
            i <- doValidate[SignedLeaseRequest](js)
            r <- doBroadcast(i.toTx)
          } yield r)
        }
      }
    }
  }

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
  def cancel: Route = path("cancel") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          mkResponse(for {
            js <- parseToEither(body)
            ri <- doValidate[SignedLeaseCancelRequest](js)
            r <- doBroadcast(ri.toTx)
          } yield r)
        }
      }
    }
  }
}
