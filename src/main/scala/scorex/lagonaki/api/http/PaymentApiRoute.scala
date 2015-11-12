package scorex.lagonaki.api.http

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.{JsError, JsSuccess, JsResult, Json}
import scorex.account.Account
import scorex.api.http._
import scorex.lagonaki.server.LagonakiApplication
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction.state.wallet.Payment
import spray.http.MediaTypes._

import scala.util.{Failure, Success, Try}

@Api(value = "/payment", description = "Payment operations.", position = 1)
case class PaymentApiRoute(application: LagonakiApplication)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {

  implicit lazy val transactionModule = application.transactionModule
  implicit lazy val wallet = application.wallet

  override lazy val route = payment

  @ApiOperation(value = "Send payment", notes = "Send payment to another wallet", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "Payment",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with response or error")
  ))
  def payment = path("payment") {
    post {
      respondWithMediaType(`application/json`) {
        entity(as[String]) { body => complete {
          walletNotExists().getOrElse {
            Try(Json.parse(body)).map { js =>
              js.validate[Payment] match {
                case err: JsError =>
                  err
                case JsSuccess(payment: Payment, _) =>
                  val txOpt = application.createPayment(payment)
                  txOpt match {
                    case Some(tx) =>
                      tx.validate() match {
                        case ValidationResult.ValidateOke =>
                          tx.json()

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
          }.toString()
        }
        }
      }
    }
  }

  // Workaround to show datatype of post request without using it in another route
  // Related: https://github.com/swagger-api/swagger-core/issues/606
  // Why is this still showing even though it's set to hidden? See https://github.com/martypitt/swagger-springmvc/issues/447
  @ApiOperation(value = "IGNORE", notes = "", hidden = true, httpMethod = "GET", response = classOf[Payment])
  protected def showPayment = Unit

}
