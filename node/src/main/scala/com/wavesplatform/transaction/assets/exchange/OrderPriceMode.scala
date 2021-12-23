package com.wavesplatform.transaction.assets.exchange

import play.api.libs.json._

sealed trait OrderPriceMode {
  private[OrderPriceMode] val jsonName: String = {
    val modeStr = this.toString.ensuring(_.nonEmpty)
    modeStr.updated(0, modeStr.charAt(0).toLower)
  }
}

object OrderPriceMode {
  case object AssetDecimals extends OrderPriceMode
  case object FixedDecimals extends OrderPriceMode

  implicit val jsonFormat: Format[OrderPriceMode] = Format(
    Reads {
      case JsString(AssetDecimals.jsonName) => JsSuccess(AssetDecimals)
      case JsString(FixedDecimals.jsonName) => JsSuccess(FixedDecimals)
      case other                            => JsError(s"Invalid mode: $other")
    },
    Writes(mode => JsString(mode.jsonName))
  )
}
