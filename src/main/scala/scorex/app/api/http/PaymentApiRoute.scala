package scorex.app.api.http

import play.api.libs.json.Json
import scorex.account.Account
import scorex.api.http._
import scorex.app.LagonakiApplication
import scorex.transaction.LagonakiTransaction.ValidationResult
import spray.routing.HttpService._

import scala.util.{Failure, Success, Try}


case class PaymentApiRoute(application:LagonakiApplication) extends ApiRoute with CommonTransactionApiFunctions {

  implicit lazy val transactionModule = application.transactionModule
  implicit lazy val wallet = application.wallet

  override lazy val route =
    path("payment") {
      post {
        entity(as[String]) { body => complete {
          walletNotExists().getOrElse {
            Try(Json.parse(body)).map { js =>
              (Try((js \ "amount").as[Long]),
                Try((js \ "fee").as[Long]),
                Try(application.wallet.privateKeyAccount((js \ "sender").as[String])),
                Try((js \ "recipient").as[String])) match {
                case (Failure(_), _, _, _) => InvalidAmount.json
                case (_, Failure(_), _, _) => InvalidFee.json
                case (_, _, Failure(_), _) => InvalidSender.json
                case (_, _, _, Failure(_)) => InvalidRecipient.json
                case (Success(_), Success(_), Success(None), Success(_)) => InvalidSender.json
                case (Success(amount), Success(fee), Success(Some(sender)), Success(recipient)) =>
                  val tx = application.createPayment(sender, new Account(recipient), amount, fee)
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
                      NegativeFee.json
                  }
              }
            }.getOrElse(WrongJson.json)
          }.toString()
        }
        }
      }
    }
}
