package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest, SignedLeaseCancelRequest, SignedLeaseRequest}
import scorex.app.Application
import scorex.transaction.{SimpleTransactionModule, StateCheckFailed}

import scala.util.{Failure, Success, Try}

@Path("/leasing")
@Api(value = "/leasing/")
case class LeaseApiRoute(application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  val settings = application.settings

  private val wallet = application.wallet
  implicit val transactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]

  override val route =
    pathPrefix("leasing") {
      lease ~ cancel ~ signedLease ~ signedCancel
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
                case JsSuccess(lease: LeaseRequest, _) =>
                  transactionModule.lease(lease, wallet) match {
                    case Success(txVal) =>
                      txVal match {
                        case Right(tx) => JsonResponse(tx.json, StatusCodes.OK)
                        case Left(e) => WrongJson().response
                      }
                    case Failure(e: StateCheckFailed) =>
                      StateCheckFailed.response
                    case _ =>
                      WrongJson().response
                  }
              }
            }.getOrElse(WrongJson().response)
          }
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
          walletNotExists(wallet).getOrElse {
            Try(Json.parse(body)).map { js =>
              js.validate[LeaseCancelRequest] match {
                case err: JsError =>
                  WrongTransactionJson(err).response
                case JsSuccess(lease: LeaseCancelRequest, _) =>
                  transactionModule.leaseCancel(lease, wallet) match {
                    case Success(txVal) =>
                      txVal match {
                        case Right(tx) => JsonResponse(tx.json, StatusCodes.OK)
                        case Left(e) => WrongJson().response
                      }
                    case Failure(e: StateCheckFailed) =>
                      StateCheckFailed.response
                    case _ =>
                      WrongJson().response
                  }
              }
            }.getOrElse(WrongJson().response)
          }
        }
      }
    }
  }

  @Path("/broadcast/lease")
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
  def signedLease: Route = path("lease") {
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

  @Path("/broadcast/cancel")
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
  def signedCancel: Route = path("cancel") {
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
