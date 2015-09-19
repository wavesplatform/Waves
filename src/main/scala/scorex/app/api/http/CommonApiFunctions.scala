package scorex.app.api.http

import akka.util.Timeout
import play.api.libs.json.{JsObject, JsValue}
import scorex.account.Account
import scorex.app.LagonakiApplication
import scorex.block.Block
import scorex.crypto.Base58

import scala.concurrent.duration._


trait CommonApiFunctions {
  implicit val timeout = Timeout(5.seconds)

  val application: LagonakiApplication

  protected[api] def walletExists(): Option[JsObject] =
    if (application.wallet.exists()) {
      Some(ApiError.json(ApiError.WalletAlreadyExists))
    } else None

  protected[api] def withBlock(encodedSignature: String)(action: Block => JsValue): JsValue =
    Base58.decode(encodedSignature).toOption.map { signature =>
      application.blockchainStorage.blockById(signature) match {
        case Some(block) => action(block)
        case None => ApiError.json(ApiError.BlockNotExists)
      }
    }.getOrElse(ApiError.json(ApiError.InvalidSignature))

  protected[api] def withPrivateKeyAccount(address: String)(action: Account => JsValue): JsValue =
    walletNotExists().getOrElse {
      if (!Account.isValidAddress(address)) {
        ApiError.json(ApiError.InvalidAddress)
      } else {
        application.wallet.privateKeyAccount(address) match {
          case None => ApiError.json(ApiError.WalletAddressNotExists)
          case Some(account) => action(account)
        }
      }
    }

  protected[api] def walletNotExists(): Option[JsObject] =
    if (!application.wallet.exists()) {
      Some(ApiError.json(ApiError.WalletNotExist))
    } else None
}