package scorex.api.http

import akka.http.scaladsl.model.StatusCodes
import play.api.libs.json.{JsObject, JsValue, Json, Reads}
import scorex.account.Account
import scorex.serialization.JsonSerializable
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction.{Transaction, TransactionModule, ValidationError}
import scorex.transaction.ValidationError._
import scorex.wallet.Wallet

import scala.util.control.Exception


trait CommonTransactionApiFunctions extends CommonApiFunctions {

  protected val transactionModule : TransactionModule[StoredInBlock]

  protected[api] def walletExists()(implicit wallet: Wallet): Option[JsObject] =
    if (wallet.exists()) Some(WalletAlreadyExists.json) else None

  protected[api] def withPrivateKeyAccount(wallet: Wallet, address: String)
                                          (action: Account => JsValue): JsonResponse =
    walletNotExists(wallet).getOrElse {
      if (!Account.isValidAddress(address)) {
        InvalidAddress.response
      } else {
        wallet.privateKeyAccount(address) match {
          case None => WalletAddressNotExists.response
          case Some(account) => JsonResponse(action(account), StatusCodes.OK)
        }
      }
    }

  protected[api] def walletNotExists(wallet: Wallet): Option[JsonResponse] =
    if (!wallet.exists()) Some(WalletNotExist.response) else None

  def jsonResponse(result: ValidationError): JsonResponse = result match {
    case ValidationError.InsufficientFee => InsufficientFee.response
    case ValidationError.NoBalance => NoBalance.response
    case ValidationError.StateCheckFailed => StateCheckFailed.response
    case ValidationError.InvalidAddress => InvalidAddress.response
    case ValidationError.NegativeAmount => InvalidAmount.response
    case ValidationError.InvalidName => InvalidName.response
    case ValidationError.InvalidSignature => InvalidSignature.response
    case ValidationError.TooBigArray => TooBigArrayAllocation.response
    case _ => Unknown.response
  }


  protected def mkResponse[A <: JsonSerializable](result: Either[ApiError, A]): JsonResponse = result match {
    case Left(e) => e.response
    case Right(r) => JsonResponse(r.json, StatusCodes.OK)
  }
  protected def parseToEither(body: String) = Exception.nonFatalCatch.either(Json.parse(body)).left.map(t => WrongJson(cause = Some(t)))
  protected def doValidate[A: Reads](js: JsValue): Either[WrongJson, A] = js.validate[A].asEither.left.map(e => WrongJson(errors = e))
  protected def doBroadcast[A <: Transaction](v: Either[ValidationError, A]) =
    v.left.map(ApiError.fromValidationError).flatMap(broadcast)
  protected def broadcast[T <: Transaction](tx: T): Either[ApiError, T] =
    if (transactionModule.onNewOffchainTransaction(tx)) Right(tx) else Left(StateCheckFailed)

}
