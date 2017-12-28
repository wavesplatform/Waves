package com.wavesplatform.matcher.model

import java.util

import com.wavesplatform.matcher.model.OrderHistoryStorage.AddressKey
import com.wavesplatform.matcher.util.MatcherDataTypes
import org.h2.mvstore.MVStore
import scorex.utils.LogMVMapBuilder

class OrderHistoryStorage(db: MVStore) {
  val orders: util.Map[String, String] = db.openMap("orders")
  val ordersInfo: util.Map[String, OrderInfo] = db.openMap("ordersInfo", new LogMVMapBuilder[String, OrderInfo]
    .valueType(MatcherDataTypes.orderInfo))
  val addressToOrderIds: util.Map[AddressKey, Array[String]] = db.openMap("addressToOrderIds")
  val addressToOrderPortfolio: util.Map[String, Map[String, Long]] = db.openMap("addressToOrderPortfolio")
}

object OrderHistoryStorage {
  type AssetAccListKey = String
  type AddressKey = String

}
