package com.wavesplatform.api.http

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.assets.exchange.Order
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
  implicit def fromValidationError(e: ValidationError): ApiError = {
    e match {
      case TxValidationError.InvalidAddress(_)               => InvalidAddress
      case TxValidationError.NegativeAmount(x, of)           => NegativeAmount(s"$x of $of")
      case TxValidationError.NonPositiveAmount(x, of)        => NonPositiveAmount(s"$x of $of")
      case TxValidationError.NegativeMinFee(x, of)           => NegativeMinFee(s"$x per $of")
      case TxValidationError.InsufficientFee(x)              => InsufficientFee(x)
      case TxValidationError.InvalidName                     => InvalidName
      case TxValidationError.InvalidSignature(_, _)          => InvalidSignature
      case TxValidationError.InvalidRequestSignature         => InvalidSignature
      case TxValidationError.TooBigArray                     => TooBigArrayAllocation
      case TxValidationError.OverflowError                   => OverflowError
      case TxValidationError.ToSelf                          => ToSelfError
      case TxValidationError.MissingSenderPrivateKey         => MissingSenderPrivateKey
      case TxValidationError.GenericError(ge)                => CustomValidationError(ge)
      case TxValidationError.AlreadyInTheState(tx, txHeight) => AlreadyInState(tx, txHeight)
      case TxValidationError.AccountBalanceError(errs)       => AccountBalanceErrors(errs)
      case TxValidationError.AliasDoesNotExist(tx)           => AliasDoesNotExist(tx)
      case TxValidationError.OrderValidationError(o, m)      => OrderInvalid(o, m)
      case TxValidationError.UnsupportedTransactionType      => UnsupportedTransactionType
      case TxValidationError.Mistiming(err)                  => Mistiming(err)
      case TxValidationError.WrongChain(ex, pr)              => InvalidChainId(ex, pr)
      case err: TxValidationError.TooManyProofs              => InvalidProofs(err.toString())
      case err: TxValidationError.ToBigProof                 => InvalidProofs(err.toString())
      case TransactionValidationError(error, tx) =>
        error match {
          case e: TxValidationError.TransactionNotAllowedByScript =>
            if (e.isAssetScript) TransactionNotAllowedByAssetScript(tx)
            else TransactionNotAllowedByAccountScript(tx)
          case TxValidationError.Mistiming(errorMessage)                         => Mistiming(errorMessage)
          case e: TxValidationError.ScriptExecutionError                         => ScriptExecutionError(tx, e.error, isTokenScript = e.isAssetScript)
          case e: TxValidationError.FailedTransactionError if e.isExecutionError => ScriptExecutionError(tx, e.message, e.isAssetScript)
          case e: TxValidationError.FailedTransactionError                       => TransactionNotAllowedByAssetScript(tx)
          case err                                                               => StateCheckFailed(tx, fromValidationError(err))
        }
      case error => CustomValidationError(error.toString)
    }
  }

  case object Unknown extends ApiError {
    override val id      = 0
    override val code    = StatusCodes.InternalServerError
    override val message = "Error is unknown"
  }

  final case class WrongJson(
      cause: Option[Throwable] = None,
      errors: scala.collection.Seq[(JsPath, scala.collection.Seq[JsonValidationError])] = Seq.empty
  ) extends ApiError {
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

  case object InvalidPublicKey extends ApiError {
    override val id      = 108
    override val code    = StatusCodes.BadRequest
    override val message = "invalid public key"
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

  final case class StateCheckFailed(tx: Transaction, errorMsg: String, details: Option[JsObject]) extends ApiError {
    override val id: Int          = StateCheckFailed.Id
    override val message: String  = StateCheckFailed.message(errorMsg)
    override val code: StatusCode = StateCheckFailed.Code
    override lazy val json        = details.fold(JsObject.empty)(identity) ++ Json.obj("error" -> id, "message" -> message, "tx" -> tx.json())
  }

  case object StateCheckFailed {
    def apply(tx: Transaction, errorMessage: String): StateCheckFailed = new StateCheckFailed(tx, errorMessage, None)
    def apply(tx: Transaction, error: ApiError): StateCheckFailed      = new StateCheckFailed(tx, error.message, Some(error.json))

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

  case class AlreadyInState(transactionId: ByteStr, height: Int) extends ApiError {
    override val id: Int          = 400
    override val code: StatusCode = StatusCodes.BadRequest
    override val message: String  = s"Transaction $transactionId is already in the state on a height of $height"
  }

  case class AccountBalanceErrors(errs: Map[Address, String]) extends ApiError {
    override val id: Int          = 402
    override val code: StatusCode = StatusCodes.BadRequest
    override val message: String  = "Accounts balance errors"
    override lazy val json: JsObject =
      Json.obj(
        "error"   -> id,
        "message" -> message,
        "details" -> Json
          .toJson(
            errs
              .map {
                case (addr, err) => addr.stringRepr -> err
              }
          )
      )
  }

  case class OrderInvalid(o: Order, error: String) extends ApiError {
    override val id: Int          = 403
    override val message: String  = s"Order validation error: $error"
    override val code: StatusCode = StatusCodes.BadRequest
    override lazy val json: JsObject = Json.obj(
      "error"   -> id,
      "message" -> message,
      "order"   -> o.json()
    )
  }

  case class InvalidChainId(expected: Byte, provided: Byte) extends ApiError {
    override val id: Int          = 404
    override val message: String  = s"Wrong chain-id. Expected - $expected, provided - $provided"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case class InvalidProofs(msg: String) extends ApiError {
    override val id: Int          = 405
    override val message: String  = msg
    override val code: StatusCode = StatusCodes.BadRequest
  }

  object ScriptErrorJson {
    def apply(errId: Int, tx: Transaction, message: String): JsObject =
      Json.obj(
        "error"       -> errId,
        "message"     -> message,
        "transaction" -> tx.json()
      )
  }

  case class InvalidTransactionId(message: String) extends ApiError {
    override val id   = 4001
    override val code = StatusCodes.BadRequest
  }

  case class InvalidBlockId(message: String) extends ApiError {
    override val id   = 4002
    override val code = StatusCodes.BadRequest
  }

  case object InvalidAssetId extends ApiError {
    override val id      = 4007
    override val message = "Invalid asset id"
    override val code    = StatusCodes.BadRequest
  }
}
