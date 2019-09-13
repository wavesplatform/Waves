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

  lazy val json = Json.obj("error" -> id, "message" -> message)
}

//noinspection TypeAnnotation
object ApiError {
  implicit def fromValidationError(e: ValidationError): ApiError =
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
          case TxValidationError.TransactionNotAllowedByScript(_, isTokenScript) =>
            if (isTokenScript) TransactionNotAllowedByAssetScript(tx)
            else TransactionNotAllowedByAccountScript(tx)
          case TxValidationError.Mistiming(errorMessage)               => Mistiming(errorMessage)
          case TxValidationError.ScriptExecutionError(err, _, isToken) => ScriptExecutionError(tx, err, isToken)
          case err                                                     => StateCheckFailed(fromValidationError(err), tx)
        }
      case error => CustomValidationError(error.toString)
    }

  case object Unknown extends ApiError {
    override val id      = 0
    override val code    = StatusCodes.InternalServerError
    override val message = "Error is unknown"
  }

  final case class WrongJson(cause: Option[Throwable] = None, errors: Seq[(JsPath, Seq[JsonValidationError])] = Seq.empty) extends ApiError {
    override val id           = 1
    override val code         = StatusCodes.BadRequest
    override lazy val message = "failed to parse json message"
    override lazy val json: JsObject = Json.obj(
      "error"            -> id,
      "message"          -> message,
      "cause"            -> cause.map(_.toString),
      "validationErrors" -> JsError.toJson(errors)
    )
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

  final case class StateCheckFailed(err: ApiError, tx: Transaction) extends ApiError {
    override val id: Int          = err.id
    override val message: String  = s"State check failed. Reason: ${err.message}"
    override val code: StatusCode = StatusCodes.BadRequest
    override lazy val json        = err.json ++ Json.obj("message" -> message, "tx" -> tx.json())
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

  final case class CustomValidationError(errorMessage: String) extends ApiError {
    override val id: Int          = 199
    override val message: String  = errorMessage
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object BlockDoesNotExist extends ApiError {
    override val id: Int         = 301
    override val code            = StatusCodes.NotFound
    override val message: String = "block does not exist"
  }

  final case class AliasDoesNotExist(aoa: AddressOrAlias) extends ApiError {
    override val id: Int = 302
    override val code    = StatusCodes.NotFound

    private[this] lazy val msgReason = aoa match {
      case a: Address => s"for address '${a.stringRepr}'"
      case a: Alias   => s"'${a.stringRepr}'"
    }
    override lazy val message: String = s"alias $msgReason doesn't exist"
  }

  final case class Mistiming(errorMessage: String) extends ApiError {
    override val id: Int          = Mistiming.Id
    override val message: String  = errorMessage
    override val code: StatusCode = StatusCodes.BadRequest
  }

  object Mistiming {
    val Id = 303
  }

  case object DataKeyDoesNotExist extends ApiError {
    override val id: Int         = 304
    override val code            = StatusCodes.NotFound
    override val message: String = "no data for this key"
  }

  final case class ScriptCompilerError(errorMessage: String) extends ApiError {
    override val id: Int          = 305
    override val code: StatusCode = StatusCodes.BadRequest
    override val message: String  = errorMessage
  }

  final case class ScriptExecutionError(tx: Transaction, error: String, isTokenScript: Boolean) extends ApiError {
    override val id: Int             = 306
    override val code: StatusCode    = StatusCodes.BadRequest
    override val message: String     = s"Error while executing ${if (isTokenScript) "token" else "account"}-script: $error"
    override lazy val json: JsObject = ScriptErrorJson(id, tx, message)
  }

  final case class TransactionNotAllowedByAccountScript(tx: Transaction) extends ApiError {
    override val id: Int             = TransactionNotAllowedByAccountScript.ErrorCode
    override val code: StatusCode    = StatusCodes.BadRequest
    override val message: String     = s"Transaction is not allowed by account-script"
    override lazy val json: JsObject = ScriptErrorJson(id, tx, message)
  }

  object TransactionNotAllowedByAccountScript {
    val ErrorCode = 307
  }

  final case class TransactionNotAllowedByAssetScript(tx: Transaction) extends ApiError {
    override val id: Int             = TransactionNotAllowedByAssetScript.ErrorCode
    override val code: StatusCode    = StatusCodes.BadRequest
    override val message: String     = s"Transaction is not allowed by token-script"
    override lazy val json: JsObject = ScriptErrorJson(id, tx, message)
  }

  object TransactionNotAllowedByAssetScript {
    val ErrorCode = 308
  }

  final case class SignatureError(error: String) extends ApiError {
    override val id: Int          = 309
    override val code: StatusCode = StatusCodes.InternalServerError
    override val message: String  = s"Signature error: $error"
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

  case object NoBalance extends ApiError {
    override val id: Int          = 2
    override val message: String  = "not enough balance"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  final case class NegativeAmount(msg: String) extends ApiError {
    override val id: Int          = 111
    override val message: String  = s"negative amount: $msg"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  final case class InsufficientFee(override val message: String = "insufficient fee") extends ApiError {
    override val id: Int          = 112
    override val code: StatusCode = StatusCodes.BadRequest
  }

  final case class WrongTransactionJson(err: JsError) extends ApiError {
    override val id: Int = 113
    override val message: String =
      err.errors.map(e => s"Validation failed for field '${e._1}', errors:${e._2}. ").mkString("\n")
    override val code: StatusCode = StatusCodes.UnprocessableEntity
  }

  final case class NegativeMinFee(msg: String) extends ApiError {
    override val id: Int          = 114
    override val message: String  = s"negative fee per: $msg"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  final case class NonPositiveAmount(msg: String) extends ApiError {
    override val id: Int          = 115
    override val message: String  = s"non-positive amount: $msg"
    override val code: StatusCode = StatusCodes.BadRequest
  }

  case object UnsupportedTransactionType extends ApiError {
    override val id: Int         = 312
    override val code            = StatusCodes.NotImplemented
    override val message: String = "transaction type not supported"
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
}
