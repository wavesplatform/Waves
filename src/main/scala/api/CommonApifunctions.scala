package api

import controller.Controller
import play.api.libs.json.JsObject


trait CommonApifunctions {

  protected[api] def walletNotExists(): Option[JsObject] =
    if (!Controller.doesWalletExists) {
      Some(ApiError.toJson(ApiError.ERROR_WALLET_NO_EXISTS))
    } else None


  protected[api] def walletNotExistsOrLocked(): Option[JsObject] =
    if (!Controller.doesWalletExists) {
      Some(ApiError.toJson(ApiError.ERROR_WALLET_NO_EXISTS))
    } else if (!Controller.isWalletUnlocked) {
      Some(ApiError.toJson(ApiError.ERROR_WALLET_LOCKED))
    } else None

}
