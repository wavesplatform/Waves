package api.http

import akka.util.Timeout
import controller.Controller
import play.api.libs.json.{JsObject, JsValue}
import scorex.account.Account
import scorex.block.Block
import scorex.crypto.{Base58, Crypto}
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.util.Try


trait CommonApifunctions {
  implicit val timeout = Timeout(5.seconds)

  protected[api] def walletExists(): Option[JsObject] =
    if (Controller.wallet.exists) {
      Some(ApiError.toJson(ApiError.ERROR_WALLET_ALREADY_EXISTS))
    } else None

  protected[api] def withBlock(encodedSignature: String)(action: Block => JsValue): JsValue =
    Try {
      Base58.decode(encodedSignature)
    }.toOption.map { signature =>
      PrunableBlockchainStorage.blockByHeader(signature) match {
        case Some(block) => action(block)
        case None => ApiError.toJson(ApiError.ERROR_BLOCK_NO_EXISTS)
      }
    }.getOrElse(ApiError.toJson(ApiError.ERROR_INVALID_SIGNATURE))

  protected[api] def withAccount(address: String)(action: Account => JsValue): JsValue =
    walletNotExists().getOrElse {
      if (!Crypto.isValidAddress(address)) {
        ApiError.toJson(ApiError.ERROR_INVALID_ADDRESS)
      } else {
        Controller.wallet.privateKeyAccount(address) match {
          case None => ApiError.toJson(ApiError.ERROR_WALLET_ADDRESS_NO_EXISTS)
          case Some(account) => action(account)
        }
      }
    }

  protected[api] def walletNotExists(): Option[JsObject] =
    if (!Controller.wallet.exists()) {
      Some(ApiError.toJson(ApiError.ERROR_WALLET_NO_EXISTS))
    } else None
}
