package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.{JsError, JsSuccess, Json}
import scorex.app.Application
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction.SimpleTransactionModule
import scorex.transaction.state.wallet.Payment

import scala.util.Try

@Path("/payment")
@Api(value = "/payment", description = "Payment operations.", position = 1)
case class PaymentApiRoute(override val application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  // TODO asInstanceOf
  implicit lazy val transactionModule: SimpleTransactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]
  lazy val wallet = application.wallet

  override lazy val route = payment

  @ApiOperation(value = "Send payment",
    notes = "Send payment to another wallet",
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
                  WrongTransactionJson(err).json
                case JsSuccess(payment: Payment, _) =>
                  val txOpt = transactionModule.createPayment(payment, wallet)
                  txOpt match {
                    case Some(tx) =>
                      tx.validate match {
                        case ValidationResult.ValidateOke =>
                          tx.json

                        case ValidationResult.InvalidAddress =>
                          InvalidAddress.json

                        case ValidationResult.NegativeAmount =>
                          NegativeAmount.json

                        case ValidationResult.NegativeFee =>
                          NegativeFee.json

                        case ValidationResult.NoBalance =>
                          NoBalance.json
                      }
                    case None =>
                      InvalidSender.json
                  }
              }
            }.getOrElse(WrongJson.json)
          }
        }
      }
    }
  }
}