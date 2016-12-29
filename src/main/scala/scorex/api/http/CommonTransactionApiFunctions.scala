package scorex.api.http

import akka.http.scaladsl.model.StatusCodes
import play.api.libs.json.{JsObject, JsValue}
import scorex.account.Account
import scorex.transaction.ValidationResult
import scorex.transaction.ValidationResult._
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

  def jsonResponse(result: ValidationResult): JsonResponse = result match {
    case ValidationResult.InsufficientFee => InsufficientFee.response
    case ValidationResult.NoBalance => NoBalance.response
    case ValidationResult.StateCheckFailed => StateCheckFailed.response
    case ValidationResult.InvalidAddress => InvalidAddress.response
    case ValidationResult.NegativeAmount => InvalidAmount.response
    case ValidationResult.InvalidName => InvalidName.response
    case ValidationResult.InvalidSignature => InvalidSignature.response
    case ValidationResult.TooBigArray => TooBigArrayAllocation.response
    case _ => Unknown.response
  }
}
