package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.model.{OrderInfo, OrderStatus}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

class TestOrderDB(maxOrdersPerRequest: Int) extends OrderDB {
  private var knownOrders   = Map.empty[ByteStr, Order]
  private var orderInfo     = Map.empty[ByteStr, OrderInfo[OrderStatus.Final]]
  private var idsForPair    = Map.empty[(Address, AssetPair), Seq[ByteStr]].withDefaultValue(Seq.empty)
  private var idsForAddress = Map.empty[Address, Seq[ByteStr]].withDefaultValue(Seq.empty)

  override def containsInfo(id: ByteStr): Boolean = orderInfo.contains(id)

  override def status(id: ByteStr): OrderStatus.Final = orderInfo.get(id).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status)

  override def saveOrderInfo(id: ByteStr, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit = if (!containsInfo(id)) {
    orderInfo += id                      -> oi
    idsForAddress += sender              -> (id +: idsForAddress(sender))
    idsForPair += (sender, oi.assetPair) -> (id +: idsForPair(sender -> oi.assetPair))
  }

  override def saveOrder(o: Order): Unit = knownOrders += o.id() -> o

  override def loadRemainingOrders(owner: Address,
                                   maybePair: Option[AssetPair],
                                   activeOrders: Seq[(ByteStr, OrderInfo[OrderStatus])]): Seq[(ByteStr, OrderInfo[OrderStatus])] =
    activeOrders ++ (for {
      id   <- maybePair.fold(idsForAddress(owner))(p => idsForPair(owner -> p))
      info <- orderInfo.get(id)
    } yield id -> info)
      .sortBy { case (_, oi) => -oi.timestamp }
      .take(maxOrdersPerRequest - activeOrders.length)
}
