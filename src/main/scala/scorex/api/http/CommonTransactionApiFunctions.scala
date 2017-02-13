package scorex.api.http

import akka.http.scaladsl.model.StatusCodes
import play.api.libs.json.JsValue
import scorex.account.Account
import scorex.wallet.Wallet


trait CommonTransactionApiFunctions extends CommonApiFunctions {

  protected[api] def withPrivateKeyAccount(wallet: Wallet, address: String)
                                          (action: Account => JsValue): JsonResponse =
      if (!Account.isValidAddress(address)) {
        InvalidAddress.response
      } else {
        wallet.privateKeyAccount(address) match {
          case None => WalletAddressNotExists.response
          case Some(account) => JsonResponse(action(account), StatusCodes.OK)
        }
      }
}
