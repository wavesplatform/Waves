package com.wavesplatform.matcher.model

import java.util

import com.wavesplatform.matcher.model.OrderHistoryStorage.{AssetPairAddressKey, AssetPairAddressListKey}
import org.h2.mvstore.MVStore
import scorex.transaction.assets.exchange.AssetPair

import scala.collection.SortedSet

class OrderHistoryStorage(db: MVStore) {
  val orders: util.Map[String, String] = db.openMap("orders")
  val ordersInfo: util.Map[String, String] = db.openMap("ordersInfo")
  val pairAddressToOrderIds: util.Map[AssetPairAddressKey, Set[String]] = db.openMap("pairAddressToOrderIds")
  val addressToOrderPortfolio: util.Map[String, Map[String, Long]] = db.openMap("addressToOrderPortfolio")
}

object OrderHistoryStorage {
  type AssetAccListKey = String
  type AssetPairAddressKey = String
  type AssetPairAddressListKey = String

  def assetPairAddressKey(assetPair: AssetPair, address: String): AssetPairAddressKey = assetPair.key + address
}