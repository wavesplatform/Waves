package scorex.app.api.http

import play.api.libs.json.Json
import scorex.account.Account
import scorex.app.LagonakiApplication
import scorex.transaction.LagonakiTransaction.ValidationResult
import spray.routing.HttpService

import scala.util.{Failure, Success, Try}


trait PaymentHttpService extends HttpService with CommonApiFunctions {

  val application:LagonakiApplication

  implicit lazy val transactionModule = application.transactionModule

  lazy val paymentRouting =
    path("payment") {
      post {
        entity(as[String]) { body => complete {
          walletNotExists().getOrElse {
            Try(Json.parse(body)).map { js =>
              (Try((js \ "amount").as[Long]),
                Try((js \ "fee").as[Long]),
                Try(application.wallet.privateKeyAccount((js \ "sender").as[String])),
                Try((js \ "recipient").as[String])) match {
                case (Failure(_), _, _, _) => ApiError.json(ApiError.InvalidAmount)
                case (_, Failure(_), _, _) => ApiError.json(ApiError.InvalidFee)
                case (_, _, Failure(_), _) => ApiError.json(ApiError.InvalidSender)
                case (_, _, _, Failure(_)) => ApiError.json(ApiError.InvalidRecipient)
                case (Success(_), Success(_), Success(None), Success(_)) => ApiError.json(ApiError.InvalidSender)
                case (Success(amount), Success(fee), Success(Some(sender)), Success(recipient)) =>
                  val tx = application.createPayment(sender, new Account(recipient), amount, fee)
                  tx.validate() match {
                    case ValidationResult.ValidateOke =>
                      tx.json()

                    case ValidationResult.InvalidAddress =>
                      ApiError.json(ApiError.InvalidAddress)

                    case ValidationResult.NegativeAmount =>
                      ApiError.json(ApiError.NegativeAmount)

                    case ValidationResult.NegativeFee =>
                      ApiError.json(ApiError.NegativeFee)

                    case ValidationResult.NoBalance =>
                      ApiError.json(ApiError.NegativeFee)
                  }
              }
            }.getOrElse(ApiError.json(ApiError.WrongJson))
          }.toString()
        }
        }
      }
    }
}
