package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scorex.app.RunnableApplication
import scorex.transaction.state.wallet.{Payment, TransferRequest}
import scorex.transaction.{SimpleTransactionModule, ValidationError}

import scala.util.Try

@Path("/payment")
@Api(value = "/payment", description = "Payment operations.", position = 1)
@Deprecated
case class PaymentApiRoute(application: RunnableApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {
  val settings = application.settings

  // TODO asInstanceOf
  implicit lazy val transactionModule: SimpleTransactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]
  lazy val wallet = application.wallet

  override lazy val route = payment

  @Deprecated
  @ApiOperation(value = "Send payment. Deprecated: use /assets/transfer instead",
    notes = "Send payment to another wallet. Deprecated: use /assets/transfer instead",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.Payment",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error")
  ))
  def payment: Route = path("payment") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          walletNotExists(wallet).getOrElse {
            Try(Json.parse(body)).map { js =>
              js.validate[Payment] match {
                case err: JsError =>
                  WrongTransactionJson(err).response
                case JsSuccess(p: Payment, _) =>
                  val transferRequest = TransferRequest(None, None, p.amount, p.fee, p.sender, "", p.recipient)
                  val txOpt = transactionModule.transferAsset(transferRequest, wallet).toOption
                  txOpt match {
                    case Some(paymentVal) =>
                      paymentVal match {
                        case Right(tx) =>
                          JsonResponse(tx.json, StatusCodes.OK)
                        case Left(err) =>
                          err match {
                            case ValidationError.InvalidAddress =>
                              InvalidAddress.response

                            case ValidationError.NegativeAmount =>
                              NegativeAmount.response

                            case ValidationError.InsufficientFee =>
                              InsufficientFee.response

                            case ValidationError.NoBalance =>
                              NoBalance.response
                          }
                      }
                    case None =>
                      InvalidSender.response
                  }
              }
            }.getOrElse(WrongJson.response)
          }
        }
      }
    }
  }
}