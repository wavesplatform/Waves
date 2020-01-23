package com.wavesplatform.api.http

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.{Transaction, _}
import play.api.libs.json._

case class ApiErrorResponse(error: Int, message: String)

object ApiErrorResponse {
  implicit val toFormat: Reads[ApiErrorResponse] = Json.reads[ApiErrorResponse]
}

trait ApiError {
  val id: Int
  val message: String
  val code: StatusCode

  lazy val json: JsObject = Json.obj("error" -> id, "message" -> message)
}

//noinspection TypeAnnotation
object ApiError {
  implicit def fromValidationError(e: ValidationError): ApiError =
    e match {
      case TxValidationError.InvalidAddress(_)        => InvalidAddress
      case TxValidationError.NegativeAmount(x, of)    => NegativeAmount(s"$x of $of")
      case TxValidationError.NonPositiveAmount(x, of) => NonPositiveAmount(s"$x of $of")
      case TxValidationError.NegativeMinFee(x, of)    => NegativeMinFee(s"$x per $of")
      case TxValidationError.InsufficientFee(x)       => InsufficientFee(x)
      case TxValidationError.InvalidName              => InvalidName
      case TxValidationError.InvalidSignature(_, _)   => InvalidSignature
      case TxValidationError.InvalidRequestSignature  => InvalidSignature
      case TxValidationError.TooBigArray              => TooBigArrayAllocation
      case TxValidationError.OverflowError            => OverflowError
      case TxValidationError.ToSelf                   => ToSelfError
      case TxValidationError.MissingSenderPrivateKey  => MissingSenderPrivateKey
      case TxValidationError.GenericError(ge)         => CustomValidationError(ge)
      case TxValidationError.AlreadyInTheState(tx, txHeight) =>
        CustomValidationError(s"Transaction $tx is already in the state on a height of $txHeight")
      case TxValidationError.AccountBalanceError(errs)  => CustomValidationError(errs.values.mkString(", "))
      case TxValidationError.AliasDoesNotExist(tx)      => AliasDoesNotExist(tx)
      case TxValidationError.OrderValidationError(_, m) => CustomValidationError(m)
      case TxValidationError.UnsupportedTransactionType => UnsupportedTransactionType
      case TxValidationError.Mistiming(err)             => Mistiming(err)
      case TransactionValidationError(error, tx) =>
        error match {
          case TxValidationError.Mistiming(errorMessage) => Mistiming(errorMessage)
          case TxValidationError.TransactionNotAllowedByScript(_, isTokenScript) =>
            if (isTokenScript) TransactionNotAllowedByAssetScript(tx)
            else TransactionNotAllowedByAccountScript(tx)
          case TxValidationError.ScriptExecutionError(err, _, isToken) =>
            ScriptExecutionError(tx, err, isToken)
          case _ => StateCheckFailed(tx, fromValidationError(error).message)
        }
      case error => CustomValidationError(error.toString)
    }

  case object Unknown extends ApiError {
    override val id      = 0
    override val code    = StatusCodes.InternalServerError
    override val message = "Error is unknown"
  }

  final case class WrongJson(cause: Option[Throwable] = None, errors: Seq[(JsPath, Seq[JsonValidationError])] = Seq.empty) extends ApiError {
    override val id              = WrongJson.Id
    override val code            = StatusCodes.BadRequest
    override val message: String = WrongJson.Message
    override lazy val json: JsObject = Json.obj(
      "error"            -> id,
      "message"          -> message,
      "cause"            -> cause.map(_.toString),
      "validationErrors" -> JsError.toJson(errors)
    )
  }
  case object WrongJson {
    val Id      = 1
    val Message = "failed to parse json message"
  }

  //API Auth
  case object ApiKeyNotValid extends ApiError {
    override val id              = 2
    override val code            = StatusCodes.Forbidden
    override val message: String = "Provided API key is not correct"
  }

  case object DiscontinuedApi extends ApiError {
    override val id      = 3
    override val code    = StatusCodes.BadRequest
    override val message = "This API is no longer supported"
  }

  case object TooBigArrayAllocation extends ApiError {
    override val id: Int          = 10
    override val message: String  = "Too big sequences requested"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  //VALIDATION
  case object InvalidSignature extends ApiError {
    override val id      = 101
    override val code    = StatusCodes.BadRequest
    override val message = "invalid signature"
  }

  case object InvalidAddress extends ApiError {
    override val id      = 102
    override val code    = StatusCodes.BadRequest
    override val message = "invalid address"
  }

  case object InvalidSeed extends ApiError {
    override val id      = 103
    override val code    = StatusCodes.BadRequest
    override val message = "invalid seed"
  }

  case object InvalidAmount extends ApiError {
    override val id      = 104
    override val code    = StatusCodes.BadRequest
    override val message = "invalid amount"
  }

  case object InvalidFee extends ApiError {
    override val id      = 105
    override val code    = StatusCodes.BadRequest
    override val message = "invalid fee"
  }

  case object InvalidSender extends ApiError {
    override val id      = 106
    override val code    = StatusCodes.BadRequest
    override val message = "invalid sender"
  }

  case object InvalidRecipient extends ApiError {
    override val id      = 107
    override val code    = StatusCodes.BadRequest
    override val message = "invalid recipient"
  }

  case object InvalidPublicKey extends ApiError {
    override val id      = 108
    override val code    = StatusCodes.BadRequest
    override val message = "invalid public key"
  }

  case object InvalidNotNumber extends ApiError {
    override val id      = 109
    override val code    = StatusCodes.BadRequest
    override val message = "argument is not a number"
  }

  case object InvalidMessage extends ApiError {
    override val id      = 110
    override val code    = StatusCodes.BadRequest
    override val message = "invalid message"
  }

  case object InvalidName extends ApiError {
    override val id: Int          = 111
    override val message: String  = "invalid name"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  final case class StateCheckFailed(tx: Transaction, err: String) extends ApiError {
    override val id: Int          = StateCheckFailed.Id
    override val message: String  = StateCheckFailed.message(err)
    override val code: StatusCode = StateCheckFailed.Code
    override lazy val json        = Json.obj("error" -> id, "message" -> message, "tx" -> tx.json())
  }

  case object StateCheckFailed {
    val Id            = 112
    val MessagePrefix = "State check failed. Reason:"
    val Code          = StatusCodes.BadRequest

    def message(err: String): String = s"${StateCheckFailed.MessagePrefix} $err"
  }

  case object OverflowError extends ApiError {
    override val id: Int          = 113
    override val message: String  = "overflow error"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object ToSelfError extends ApiError {
    override val id: Int          = 114
    override val message: String  = "Transaction to yourself"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object MissingSenderPrivateKey extends ApiError {
    override val id: Int          = 115
    override val message: String  = "no private key for sender address in wallet"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case class InvalidIds(ids: Seq[String]) extends ApiError {
    override val id: Int          = InvalidIds.Id
    override val message: String  = s"Request contains invalid IDs. ${ids.mkString(", ")}"
    override val code: StatusCode = StatusCodes.BadRequest

    override lazy val json: JsObject = Json.obj("error" -> id, "message" -> message, "ids" -> ids)
  }

  case object InvalidIds {
    val Id = 116
  }

  final case class CustomValidationError(errorMessage: String) extends ApiError {
    override val id: Int          = CustomValidationError.Id
    override val message: String  = errorMessage
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object CustomValidationError {
    val Id = 199
  }

  case object BlockDoesNotExist extends ApiError {
    override val id: Int         = 301
    override val code            = StatusCodes.NotFound
    override val message: String = "block does not exist"
  }

  final case class AliasDoesNotExist(aoa: AddressOrAlias) extends ApiError {
    override val id: Int = AliasDoesNotExist.Id
    override val code    = StatusCodes.NotFound

    private[this] lazy val msgReason = aoa match {
      case a: Address => s"for address '${a.stringRepr}'"
      case a: Alias   => s"'${a.stringRepr}'"
    }
    override lazy val message: String = s"alias $msgReason doesn't exist"
  }

  case object AliasDoesNotExist {
    val Id = 302
  }

  final case class Mistiming(errorMessage: String) extends ApiError {
    override val id: Int          = Mistiming.Id
    override val message: String  = errorMessage
    override val code: StatusCode = Mistiming.Code
  }

  object Mistiming {
    val Id               = 303
    val Code: StatusCode = StatusCodes.BadRequest
  }

  case object DataKeyDoesNotExist extends ApiError {
    override val id: Int         = 304
    override val code            = StatusCodes.NotFound
    override val message: String = "no data for this key"
  }

  final case class ScriptCompilerError(errorMessage: String) extends ApiError {
    override val id: Int          = ScriptCompilerError.Id
    override val code: StatusCode = StatusCodes.BadRequest
    override val message: String  = errorMessage
  }

  case object ScriptCompilerError {
    val Id = 305
  }

  final case class ScriptExecutionError(tx: Transaction, error: String, isTokenScript: Boolean) extends ApiError {
    override val id: Int             = ScriptExecutionError.Id
    override val code: StatusCode    = StatusCodes.BadRequest
    override val message: String     = s"Error while executing ${if (isTokenScript) "token" else "account"}-script: $error"
    override lazy val json: JsObject = ScriptErrorJson(id, tx, message)
  }

  case object ScriptExecutionError {
    val Id = 306
  }

  final case class TransactionNotAllowedByAccountScript(tx: Transaction) extends ApiError {
    override val id: Int             = TransactionNotAllowedByAccountScript.Id
    override val code: StatusCode    = StatusCodes.BadRequest
    override val message: String     = s"Transaction is not allowed by account-script"
    override lazy val json: JsObject = ScriptErrorJson(id, tx, message)
  }

  object TransactionNotAllowedByAccountScript {
    val Id = 307
  }

  final case class TransactionNotAllowedByAssetScript(tx: Transaction) extends ApiError {
    override val id: Int             = TransactionNotAllowedByAssetScript.Id
    override val code: StatusCode    = TransactionNotAllowedByAssetScript.Code
    override val message: String     = TransactionNotAllowedByAssetScript.Message
    override lazy val json: JsObject = ScriptErrorJson(id, tx, message)
  }

  object TransactionNotAllowedByAssetScript {
    val Id      = 308
    val Message = s"Transaction is not allowed by token-script"
    val Code    = StatusCodes.BadRequest
  }

  final case class SignatureError(error: String) extends ApiError {
    override val id: Int          = SignatureError.Id
    override val code: StatusCode = StatusCodes.InternalServerError
    override val message: String  = s"Signature error: $error"
  }

  case object SignatureError {
    val Id = 309
  }

  case object WalletNotExist extends ApiError {
    override val id: Int          = 201
    override val message: String  = "wallet does not exist"
    override val code: StatusCode = StatusCodes.NotFound
  }

  case object WalletAddressDoesNotExist extends ApiError {
    override val id: Int          = 202
    override val message: String  = "private key for the public key does not exist in wallet"
    override val code: StatusCode = StatusCodes.NotFound
  }

  case object WalletLocked extends ApiError {
    override val id: Int          = 203
    override val message: String  = "wallet is locked"
    override val code: StatusCode = StatusCodes.UnprocessableEntity
  }

  case object WalletAlreadyExists extends ApiError {
    override val id: Int          = 204
    override val message: String  = "wallet already exists"
    override val code: StatusCode = StatusCodes.Conflict
  }

  case object WalletSeedExportFailed extends ApiError {
    override val id: Int          = 205
    override val message: String  = "seed exporting failed"
    override val code: StatusCode = StatusCodes.InternalServerError
  }

  //TRANSACTIONS
  case object TransactionDoesNotExist extends ApiError {
    override val id: Int          = 311
    override val message: String  = "transactions does not exist"
    override val code: StatusCode = StatusCodes.NotFound
  }

  case object UnsupportedTransactionType extends ApiError {
    override val id: Int         = 312
    override val code            = StatusCodes.NotImplemented
    override val message: String = "transaction type not supported"
  }

  case object NoBalance extends ApiError {
    override val id: Int          = 313
    override val message: String  = "not enough balance"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  final case class NegativeAmount(msg: String) extends ApiError {
    override val id: Int          = NegativeAmount.Id
    override val message: String  = s"negative amount: $msg"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object NegativeAmount {
    val Id = 111
  }

  final case class InsufficientFee(override val message: String = "insufficient fee") extends ApiError {
    override val id: Int          = InsufficientFee.Id
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object InsufficientFee {
    val Id = 112
  }

  final case class WrongTransactionJson(err: JsError) extends ApiError {
    override val id: Int = WrongTransactionJson.Id
    override val message: String =
      err.errors.map(e => s"Validation failed for field '${e._1}', errors:${e._2}. ").mkString("\n")
    override val code: StatusCode = StatusCodes.UnprocessableEntity
  }

  case object WrongTransactionJson {
    val Id = 113
  }

  final case class NegativeMinFee(msg: String) extends ApiError {
    override val id: Int          = NegativeMinFee.Id
    override val message: String  = s"negative fee per: $msg"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object NegativeMinFee {
    val Id = 114
  }

  final case class NonPositiveAmount(msg: String) extends ApiError {
    override val id: Int          = NonPositiveAmount.Id
    override val message: String  = s"non-positive amount: $msg"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object NonPositiveAmount {
    val Id = 115
  }

  private object ScriptErrorJson {
    def apply(errId: Int, tx: Transaction, message: String): JsObject =
      Json.obj(
        "error"       -> errId,
        "message"     -> message,
        "transaction" -> tx.json()
      )
  }
}
