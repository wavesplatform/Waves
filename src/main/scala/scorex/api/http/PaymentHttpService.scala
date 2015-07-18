package scorex.api.http

import scorex.controller.Controller
import play.api.libs.json.Json
import scorex.account.Account
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.TransactionCreator
import spray.routing.HttpService

import scala.util.{Failure, Success, Try}


trait PaymentHttpService extends HttpService with CommonApifunctions {
  lazy val paymentRouting =
    path("payment") {
      post {
        entity(as[String]) { body => complete {
          walletNotExists().getOrElse {
            Try(Json.parse(body)).map { js =>
              (Try((js \ "amount").as[Long]),
                Try((js \ "fee").as[Long]),
                Try(Controller.wallet.privateKeyAccount((js \ "sender").as[String])),
                Try((js \ "recipient").as[String])) match {
                case (Failure(_), _, _, _) => ApiError.toJson(ApiError.ERROR_INVALID_AMOUNT)
                case (_, Failure(_), _, _) => ApiError.toJson(ApiError.ERROR_INVALID_FEE)
                case (_, _, Failure(_), _) => ApiError.toJson(ApiError.ERROR_INVALID_SENDER)
                case (_, _, _, Failure(_)) => ApiError.toJson(ApiError.ERROR_INVALID_RECIPIENT)
                case (Success(_), Success(_), Success(None), Success(_)) => ApiError.toJson(ApiError.ERROR_INVALID_SENDER)
                case (Success(amount), Success(fee), Success(Some(sender)), Success(recipient)) =>
                  val tx = TransactionCreator.createPayment(sender, new Account(recipient), amount, fee)
                  tx.validate() match {
                    case ValidationResult.VALIDATE_OKE =>
                      tx.toJson()

                    case ValidationResult.INVALID_ADDRESS =>
                      ApiError.toJson(ApiError.ERROR_INVALID_ADDRESS)

                    case ValidationResult.NEGATIVE_AMOUNT =>
                      ApiError.toJson(ApiError.ERROR_NEGATIVE_AMOUNT)

                    case ValidationResult.NEGATIVE_FEE =>
                      ApiError.toJson(ApiError.ERROR_NEGATIVE_FEE)

                    case ValidationResult.NO_BALANCE =>
                      ApiError.toJson(ApiError.ERROR_NEGATIVE_FEE)
                  }
              }
            }.getOrElse(ApiError.toJson(ApiError.ERROR_JSON))
          }.toString()
        }
        }
      }
    }
}
