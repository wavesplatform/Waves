package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.database.{DBExt, RW}
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder.{Filled, OrderStatus}
import com.wavesplatform.matcher.{MatcherKeys, MatcherSettings, OrderAssets}
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state._
import kamon.Kamon
import org.iq80.leveldb.DB
import scorex.account.Address
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.{Order, OrderType}

class OrderHistory(db: DB, settings: MatcherSettings) {
  import com.wavesplatform.matcher.MatcherKeys._

  private val timer               = Kamon.timer("matcher.order-history.impl")
  private val saveOpenVolumeTimer = timer.refine("action" -> "save-open-volume")
  private val saveOrderInfoTimer  = timer.refine("action" -> "save-order-info")
  private val openVolumeTimer     = timer.refine("action" -> "open-volume")

  private def saveOrderInfo(rw: RW, event: Event): Map[ByteStr, (Order, OrderInfo)] =
    saveOrderInfoTimer.measure(db.readWrite { rw =>
      val updatedInfo = Events.createOrderInfo(event).map {
        case (orderId, (o, oi)) => (orderId, (o, DBUtils.orderInfo(rw, orderId).combine(oi)))
      }

      for ((orderId, (_, oi)) <- updatedInfo) {
        rw.put(MatcherKeys.orderInfo(orderId), oi)
      }

      updatedInfo
    })

  def openVolume(address: Address, assetId: Option[AssetId]): Long =
    openVolumeTimer.measure(db.get(MatcherKeys.openVolume(address, assetId)).getOrElse(0L))

  private def saveOpenVolume(rw: RW, event: Map[Address, OpenPortfolio]): Unit = saveOpenVolumeTimer.measure {
    for ((address, op) <- event) {
      val newAssets = Set.newBuilder[Option[AssetId]]
      for ((assetId, amount) <- op.orders if amount != 0) {
        val k = MatcherKeys.openVolume(address, assetId)
        val newValue = safeSum(amount, rw.get(k) match {
          case None =>
            newAssets += assetId
            0L
          case Some(v) => v
        })

        rw.put(k, Some(newValue))
      }

      val r = newAssets.result()
      if (r.nonEmpty) {
        val k         = openVolumeSeqNr(address)
        val prevSeqNr = rw.get(k)
        for ((assetId, offset) <- r.zipWithIndex) {
          rw.put(openVolumeAsset(address, prevSeqNr + offset + 1), assetId)
        }
        rw.put(k, prevSeqNr + r.size)
      }
    }
  }

  private def saveOrder(rw: RW, order: Order): Unit = rw.put(MatcherKeys.order(order.id()), Some(order))

  def orderAccepted(event: OrderAdded): Unit = db.readWrite { rw =>
    val lo = event.order
    saveOrder(rw, lo.order)
    val updatedInfo = saveOrderInfo(rw, event)
    saveOpenVolume(rw, Events.createOpenPortfolio(event))

    // for OrderAdded events, updatedInfo contains just one element
    for ((orderId, (o, _)) <- updatedInfo) {
      val k         = MatcherKeys.addressOrdersSeqNr(o.senderPublicKey)
      val nextSeqNr = rw.get(k) + 1
      rw.put(k, nextSeqNr)

      val spendAssetId = if (o.orderType == OrderType.BUY) o.assetPair.priceAsset else o.assetPair.amountAsset
      rw.put(MatcherKeys.addressOrders(o.senderPublicKey, nextSeqNr), OrderAssets(orderId, spendAssetId))
    }
  }

  def orderExecuted(event: OrderExecuted): Unit = db.readWrite { rw =>
    saveOrder(rw, event.submitted.order)
    saveOrderInfo(rw, event)
    val v = Monoid.combine(Events.createOpenPortfolio(OrderAdded(event.submittedExecuted)), Events.createOpenPortfolio(event))
    saveOpenVolume(rw, v)
  }

  def orderCanceled(event: OrderCanceled): Unit = db.readWrite { rw =>
    saveOrderInfo(rw, event)
    saveOpenVolume(rw, Events.createOpenPortfolio(event))
  }

  def orderInfo(id: ByteStr): OrderInfo = DBUtils.orderInfo(db, id)

  def order(id: ByteStr): Option[Order] = db.get(MatcherKeys.order(id))

  def deleteOrder(address: Address, orderId: ByteStr): Boolean = db.readWrite { rw =>
    DBUtils.orderInfo(rw, orderId).status match {
      case Filled | LimitOrder.Cancelled(_) =>
        rw.delete(MatcherKeys.order(orderId))
        rw.delete(MatcherKeys.orderInfo(orderId))
        true
      case _ =>
        false
    }
  }
}

object OrderHistory {
  import OrderInfo.orderStatusOrdering

  object OrderHistoryOrdering extends Ordering[(ByteStr, OrderInfo, Option[Order])] {
    def orderBy(oh: (ByteStr, OrderInfo, Option[Order])): (OrderStatus, Long) = (oh._2.status, -oh._3.map(_.timestamp).getOrElse(0L))

    override def compare(first: (ByteStr, OrderInfo, Option[Order]), second: (ByteStr, OrderInfo, Option[Order])): Int = {
      implicitly[Ordering[(OrderStatus, Long)]].compare(orderBy(first), orderBy(second))
    }
  }
}
