package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json._
import scorex.api.http.assets.LeaseRequest
import scorex.app.Application
import scorex.transaction.{SimpleTransactionModule, StateCheckFailed}

import scala.util.{Failure, Success, Try}

@Path("/lease")
@Api(value = "/lease/")
class LeaseApiRoute(application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  val settings = application.settings

  private val wallet = application.wallet
  private val state = application.blockStorage.state
  private implicit val transactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]

  override val route =
    pathPrefix("lease") {
      lease
    }

  @Path("/lease")
  @ApiOperation(value = "Creates a lease transaction",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.PaymentRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
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
                  transactionModule.lease(r, wallet) match {
                    case Success(txVal) =>
                      txVal match {
                        case Right(tx) => JsonResponse(tx.json, StatusCodes.OK)
                        case Left(e) => WrongJson.response
                      }
                    case Failure(e: StateCheckFailed) =>
                      StateCheckFailed.response
                    case _ =>
                      WrongJson.response
                  }
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  }
}