package com.wavesplatform.transaction.assets.exchange

import play.api.libs.json.*

sealed trait OrderPriceMode {
  private[OrderPriceMode] val jsonName: String = {
    val modeStr = this.toString.ensuring(_.nonEmpty)
    modeStr.updated(0, modeStr.charAt(0).toLower)
  }
}

object OrderPriceMode {
  case object Default       extends OrderPriceMode
  case object AssetDecimals extends OrderPriceMode
  case object FixedDecimals extends OrderPriceMode

  private val byJsonName = Seq(Default, AssetDecimals, FixedDecimals).map(v => v.jsonName -> v).toMap

  implicit val jsonFormat: Format[OrderPriceMode] = Format(
    Reads {
      case JsNull                                      => JsSuccess(Default)
      case JsString(mode) if byJsonName.contains(mode) => JsSuccess(byJsonName(mode))
      case other                                       => JsError(s"Invalid mode: $other")
    },
    Writes {
      case Default => JsNull
      case other   => JsString(other.jsonName)
    }
  )
}
