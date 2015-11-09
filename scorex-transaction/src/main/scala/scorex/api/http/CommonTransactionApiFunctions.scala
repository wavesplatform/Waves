package scorex.api.http

import play.api.libs.json.{JsObject, JsValue}
import scorex.account.Account
import scorex.transaction.state.wallet.Wallet


trait CommonTransactionApiFunctions extends CommonApiFunctions {

  protected[api] def walletExists()(implicit wallet: Wallet): Option[JsObject] =
    if (wallet.exists()) Some(WalletAlreadyExists.json) else None

  protected[api] def withPrivateKeyAccount(address: String)
                                          (action: Account => JsValue)
                                          (implicit wallet: Wallet): JsValue =
    walletNotExists().getOrElse {
      if (!Account.isValidAddress(address)) {
        InvalidAddress.json
      } else {
        wallet.privateKeyAccount(address) match {
          case None => WalletAddressNotExists.json
          case Some(account) => action(account)
        }
      }
    }

  protected[api] def walletNotExists()(implicit wallet: Wallet): Option[JsObject] =
    if (!wallet.exists()) Some(WalletNotExist.json) else None
}