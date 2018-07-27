package com.wavesplatform.matcher.api

import com.wavesplatform.database.{DBExt, RW}
import com.wavesplatform.matcher.MatcherKeys
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.state.ByteStr
import org.iq80.leveldb.DB
import scorex.account.Address
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.Order
import scorex.utils.ScorexLogging

object DBUtils extends ScorexLogging {
  import OrderInfo.orderStatusOrdering

  def ordersByAddress(db: DB, address: Address, changedAssets: Set[Option[AssetId]], activeOnly: Boolean): Seq[(Order, OrderInfo)] = db.readOnly {
    ro =>
      (for {
        idx <- db.get(MatcherKeys.addressOrdersSeqNr(address)) to 1 by -1
        orderSpendAsset = ro.get(MatcherKeys.addressOrders(address, idx))
        if changedAssets.isEmpty || changedAssets(orderSpendAsset.spendAsset)
        order <- ro.get(MatcherKeys.order(orderSpendAsset.orderId))
        orderInfo = ro.get(MatcherKeys.orderInfo(orderSpendAsset.orderId))
        if !(activeOnly && orderInfo.status.isFinal)
      } yield (order, orderInfo)).sortBy { case (order, info) => (info.status, -order.timestamp) }
  }

  def reservedBalance(db: DB, address: Address): Map[Option[AssetId], Long] = db.readOnly { ro =>
    log.trace(s"Loading reserved balance for $address")
    (for {
      idx <- 1 to ro.get(MatcherKeys.openVolumeSeqNr(address))
      assetId = ro.get(MatcherKeys.openVolumeAsset(address, idx))
      volume <- ro.get(MatcherKeys.openVolume(address, assetId))
      if volume != 0
    } yield assetId -> volume).toMap
  }

  def orderInfo(db: DB, orderId: ByteStr): OrderInfo = db.get(MatcherKeys.orderInfo(orderId))
  def orderInfo(rw: RW, orderId: ByteStr): OrderInfo = rw.get(MatcherKeys.orderInfo(orderId))
}
