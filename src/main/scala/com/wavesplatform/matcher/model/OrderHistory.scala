package com.wavesplatform.matcher.model

import com.twitter.storehaus.cache.TTLCache
import com.twitter.conversions.time._

trait OrderHistory {
  val ordersRemainingAmount = TTLCache[String, Long](1.day)


}
