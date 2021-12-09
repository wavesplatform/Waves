package com.wavesplatform.transaction.assets.exchange

sealed trait OrderPriceMode
object OrderPriceMode {
  object AssetDecimals extends OrderPriceMode
  object FixedDecimals extends OrderPriceMode
}
