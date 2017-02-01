package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json._
import scorex.api.http.assets.LeaseRequest
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest}
import scorex.app.Application
import scorex.transaction.SimpleTransactionModule

import scala.util.Try

@Path("/leasing")
@Api(value = "/lease/")
case class LeaseApiRoute(application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  val settings = application.settings

  private val wallet = application.wallet
  private implicit val transactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]

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
      dataType = "scorex.transaction.state.wallet.LeaseRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"untilBlock\":\"blockId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def lease: Route = path("lease") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            Try(Json.parse(body)).map { js =>
              js.validate[LeaseRequest] match {
                case err: JsError =>
                  WrongTransactionJson(err).response
                case JsSuccess(r: LeaseRequest, _) =>
                  transactionModule.lease(r, wallet).map {
                    _.fold(ApiError.fromValidationError, { tx => JsonResponse(tx.json, StatusCodes.OK) })
                  }.getOrElse(InvalidSender.response)
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  }

  @Path("/lease")
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
          walletNotExists(wallet).getOrElse {
            Try(Json.parse(body)).map { js =>
              js.validate[LeaseCancelRequest] match {
                case err: JsError =>
                  WrongTransactionJson(err).response
                case JsSuccess(r: LeaseCancelRequest, _) =>
                  transactionModule.leaseCancel(r, wallet).map {
                    _.fold(ApiError.fromValidationError, { tx => JsonResponse(tx.json, StatusCodes.OK) })
                  }.getOrElse(InvalidSender.response)
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  }
}