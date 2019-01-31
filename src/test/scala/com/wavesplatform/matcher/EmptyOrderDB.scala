package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.matcher.model.{LimitOrder, OrderInfo}
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

object EmptyOrderDB extends OrderDB {
  override def contains(id: ByteStr): Boolean                                   = false
  override def status(id: ByteStr): LimitOrder.OrderStatus                      = LimitOrder.NotFound
  override def saveOrderInfo(id: ByteStr, sender: Address, oi: OrderInfo): Unit = {}
  override def saveOrder(o: Order): Unit                                        = {}
  override def loadRemainingOrders(owner: Address, maybePair: Option[AssetPair], activeOrders: Seq[(ByteStr, OrderInfo)]): Seq[(ByteStr, OrderInfo)] =
    Seq.empty
}
