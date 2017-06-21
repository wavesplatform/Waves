package scorex.api.http

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.state2.diffs.TransactionDiffer.TransactionValidationError
import play.api.libs.json._
import scorex.account.{Account, AccountOrAlias, Alias}
import scorex.transaction.{Transaction, ValidationError}

case class ApiErrorResponse(error: Int, message: String)

trait ApiError {
  val id: Int
  val message: String
  val code: StatusCode

  lazy val json = Json.obj("error" -> id, "message" -> message)
}


object ApiError {
  def fromValidationError(e: ValidationError): ApiError = e match {
    case ValidationError.InvalidAddress => InvalidAddress
    case ValidationError.NegativeAmount => NegativeAmount
    case ValidationError.InsufficientFee => InsufficientFee
    case ValidationError.InvalidName => InvalidName
    case ValidationError.InvalidSignature(_, _) => InvalidSignature
    case ValidationError.InvalidRequestSignature => InvalidSignature
    case ValidationError.TooBigArray => TooBigArrayAllocation
    case ValidationError.OverflowError => OverflowError
    case ValidationError.ToSelf => ToSelfError
    case ValidationError.MissingSenderPrivateKey => MissingSenderPrivateKey
    case ValidationError.GenericError(ge) => CustomValidationError(ge)
    case ValidationError.AliasNotExists(tx) => AliasNotExists(tx)
    case ValidationError.UnsupportedTransactionType => CustomValidationError("UnsupportedTransactionType")
    case ValidationError.AccountBalanceError(errs) => CustomValidationError(errs.values.mkString(", "))
    case ValidationError.OrderValidationError(order, m) => CustomValidationError(m)
    case TransactionValidationError(err,tx) => StateCheckFailed(tx, fromValidationError(err).message)
  }
}

case object Unknown extends ApiError {
  override val id = 0
  override val code = StatusCodes.InternalServerError
  override val message = "Error is unknown"
}

case class WrongJson(
                        cause: Option[Throwable] = None,
                        errors: Seq[(JsPath, Seq[JsonValidationError])] = Seq.empty) extends ApiError {
  override val id = 1
  override val code = StatusCodes.BadRequest
  override lazy val message = "failed to parse json message"
  override lazy val json: JsObject = Json.obj(
    "error" -> id,
    "message" -> message,
    "cause" -> cause.map(_.toString),
    "validationErrors" -> JsError.toJson(errors)
  )
}

//API Auth
case object ApiKeyNotValid extends ApiError {
  override val id = 2
  override val code = StatusCodes.Forbidden
  override val message: String = "Provided API key is not correct"
}

case object TooBigArrayAllocation extends ApiError {
  override val id: Int = 10
  override val message: String = "Too big sequences requested"
  override val code: StatusCode = StatusCodes.BadRequest
}


//VALIDATION
case object InvalidSignature extends ApiError {
  override val id = 101
  override val code = StatusCodes.BadRequest
  override val message = "invalid signature"
}

case object InvalidAddress extends ApiError {
  override val id = 102
  override val code = StatusCodes.BadRequest
  override val message = "invalid address"
}

case object InvalidSeed extends ApiError {
  override val id = 103
  override val code = StatusCodes.BadRequest
  override val message = "invalid seed"
}

case object InvalidAmount extends ApiError {
  override val id = 104
  override val code = StatusCodes.BadRequest
  override val message = "invalid amount"
}

case object InvalidFee extends ApiError {
  override val id = 105
  override val code = StatusCodes.BadRequest
  override val message = "invalid fee"
}

case object InvalidSender extends ApiError {
  override val id = 106
  override val code = StatusCodes.BadRequest
  override val message = "invalid sender"
}

case object InvalidRecipient extends ApiError {
  override val id = 107
  override val code = StatusCodes.BadRequest
  override val message = "invalid recipient"
}

case object InvalidPublicKey extends ApiError {
  override val id = 108
  override val code = StatusCodes.BadRequest
  override val message = "invalid public key"
}

case object InvalidNotNumber extends ApiError {
  override val id = 109
  override val code = StatusCodes.BadRequest
  override val message = "argument is not a number"
}

case object InvalidMessage extends ApiError {
  override val id = 110
  override val code = StatusCodes.BadRequest
  override val message = "invalid message"
}

case object InvalidName extends ApiError {
  override val id: Int = 111
  override val message: String = "invalid name"
  override val code: StatusCode = StatusCodes.BadRequest
}

case class StateCheckFailed(tx: Transaction, err: String) extends ApiError {
  override val id: Int = 112
  override val message: String = s"State check failed. Reason: $err"
  override val code: StatusCode = StatusCodes.BadRequest
  override lazy val json = Json.obj("error" -> id, "message" -> message, "tx" -> tx.json)
}

case object OverflowError extends ApiError {
  override val id: Int = 113
  override val message: String = "overflow error"
  override val code: StatusCode = StatusCodes.BadRequest
}

case object ToSelfError extends ApiError {
  override val id: Int = 114
  override val message: String = "Transaction to yourself"
  override val code: StatusCode = StatusCodes.BadRequest
}

case object MissingSenderPrivateKey extends ApiError {
  override val id: Int = 115
  override val message: String = "no private key for sender address in wallet"
  override val code: StatusCode = StatusCodes.BadRequest
}

case class CustomValidationError(errorMessage: String) extends ApiError {
  override val id: Int = 199
  override val message: String = errorMessage
  override val code: StatusCode = StatusCodes.BadRequest
}

case object BlockNotExists extends ApiError {
  override val id: Int = 301
  override val code = StatusCodes.NotFound
  override val message: String = "block does not exist"
}

case class AliasNotExists(aoa: AccountOrAlias) extends ApiError {
  override val id: Int = 301
  override val code = StatusCodes.NotFound
  private lazy val msgReason = aoa match {
    case a: Account => s"for address '${a.stringRepr}'"
    case a: Alias => s"'${a.stringRepr}'"
  }
  override val message: String = s"alias $msgReason doesn't exist"
}
