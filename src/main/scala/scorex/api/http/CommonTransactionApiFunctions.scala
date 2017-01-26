package scorex.api.http

import akka.http.scaladsl.model.StatusCodes
import play.api.libs.json.{JsObject, JsValue}
import scorex.account.Account
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError._
import scorex.wallet.Wallet


trait CommonTransactionApiFunctions extends CommonApiFunctions {

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
}
