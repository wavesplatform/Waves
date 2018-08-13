package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.database.{DBExt, RW}
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.Events._
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

  private def combine(order: Order, event: Event, curr: OrderInfo, diff: OrderInfoDiff): OrderInfo = {
    val r =
      if (diff.isNew) {
        val executedAmount = diff.executedAmount.getOrElse(0L)
        val remainingFee   = order.matcherFee - diff.totalExecutedFee.getOrElse(order.matcherFee)
        OrderInfo(
          amount = order.amount,
          filled = executedAmount,
          canceled = diff.nowCanceled.getOrElse(false),
          minAmount = diff.newMinAmount,
          remainingFee = remainingFee,
          remainingSpend = LimitOrder(order).partial(order.amount - executedAmount, remainingFee).getSpendAmount
        )
      } else {
        OrderInfo(
          amount = order.amount,
          filled = curr.filled + diff.executedAmount.getOrElse(0L),
          canceled = diff.nowCanceled.getOrElse(curr.canceled),
          minAmount = diff.newMinAmount.orElse(curr.minAmount),
          remainingFee = order.matcherFee - diff.totalExecutedFee.getOrElse(curr.remainingFee),
          remainingSpend = diff.remainingSpend.getOrElse(curr.remainingSpend)
        )
      }

    println(s"""
               |combine (order.id = ${order.id()}, sender = ${order.sender}):
               |  curr:     $curr
               |  diff:     $diff
               |  combined: $r
             """.stripMargin)
    r
  }

  private def saveOrderInfo(rw: RW, event: Event): Seq[Order] =
    saveOrderInfoTimer.measure(db.readWrite { rw =>
      val diff = Events.collectChanges(event)

      println(s"""
           |saveOrderInfo: ${diff.size} changes
           |""".stripMargin)
      val updatedInfo = diff.map {
        case (o, d) =>
          val orderId  = o.id()
          val curr     = DBUtils.orderInfo(rw, orderId)
          val combined = combine(o, event, curr, d)
          rw.put(MatcherKeys.orderInfo(orderId), combined)
          o
      }

      updatedInfo
    })

  def openVolume(address: Address, assetId: Option[AssetId]): Long =
    openVolumeTimer.measure(db.get(MatcherKeys.openVolume(address, assetId)).getOrElse(0L))

  private def toString(openPortfolio: OpenPortfolio): String =
    openPortfolio.orders
      .map { case (assetId, v) => s"  $assetId -> $v" }
      .mkString("\n")

  private def toString(eventDiff: Map[Address, OpenPortfolio]): String =
    eventDiff
      .map { case (addr, portfolio) => s"$addr:\n${toString(portfolio)}" }
      .mkString("\n")

  private def saveOpenVolume(rw: RW, opDiff: Map[Address, OpenPortfolio]): Unit = saveOpenVolumeTimer.measure {
    println(s"""|
                |saveOpenVolume: 
                |opDiff:
                |${toString(opDiff)}
                |""".stripMargin)

    for ((address, op) <- opDiff) {
      val newAssets = Set.newBuilder[Option[AssetId]]
      for ((assetId, amount) <- op.orders if amount != 0) {
        val k = MatcherKeys.openVolume(address, assetId)
        val orig = rw.get(k) match {
          case None =>
            newAssets += assetId
            0L
          case Some(v) => v
        }
        val newValue = safeSum(orig, amount)

        println(s"saveOpenVolume: update: $address: $assetId: $orig -> $newValue ($orig + $amount)")
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

    val prevOrderInfo    = OrderInfo(lo.order.amount, 0L, false, Some(lo.minAmountOfAmountAsset), lo.order.matcherFee, lo.getSpendAmount)
    val newOrders        = saveOrderInfo(rw, event)
    val updatedOrderInfo = orderInfo(lo.order.id())

    //val opDiff          = Events.createOpenPortfolio(event)
    val opDiff = orderInfoDiffNew(
      lo.order,
      updatedOrderInfo
    )
    val opOrderInfoDiff = orderInfoDiff(lo.order, prevOrderInfo, updatedOrderInfo)
    println(s"""|
                |orderAccepted:
                |opDiff:
                |${toString(opDiff)}
                |
                |opOrderInfoDiff:
                |${toString(opOrderInfoDiff)}
                |
                |
                |""".stripMargin)
    saveOpenVolume(rw, opDiff)

    // for OrderAdded events, updatedInfo contains just one element
    for (o <- newOrders) {
      val k         = MatcherKeys.addressOrdersSeqNr(o.senderPublicKey)
      val nextSeqNr = rw.get(k) + 1
      rw.put(k, nextSeqNr)

      val spendAssetId = if (o.orderType == OrderType.BUY) o.assetPair.priceAsset else o.assetPair.amountAsset
      rw.put(MatcherKeys.addressOrders(o.senderPublicKey, nextSeqNr), Some(OrderAssets(o.id(), spendAssetId)))
    }
  }

  def toString(x: OrderInfo): String = {
    s"""|$x
        |remaining: ${x.remaining}
        |remainingFee: ${x.remainingFee}""".stripMargin
  }

  def orderExecuted(event: OrderExecuted): Unit = db.readWrite { rw =>
    saveOrder(rw, event.submitted.order)
    val prevCounterInfo = orderInfo(event.counter.order.id())
    val prevSubmittedInfo = OrderInfo(event.submitted.order.amount,
                                      0L,
                                      false,
                                      Some(event.submitted.minAmountOfAmountAsset),
                                      event.submitted.order.matcherFee,
                                      event.submitted.getSpendAmount)
    saveOrderInfo(rw, event)
    val updatedCounterInfo   = orderInfo(event.counter.order.id())
    val updatedSubmittedInfo = orderInfo(event.submitted.order.id())

    val submittedAddedOpDiff = Events.createOpenPortfolio(OrderAdded(event.submitted)) // ?
    val eventDiff            = Events.createOpenPortfolio(event)                       // ?
    //val diff            = createOpenPortfolio(event)
    val opDiff = Monoid
      .combine(submittedAddedOpDiff, eventDiff)
//      .updated(
//        event.counter.order.senderPublicKey.toAddress,
//        opOrderInfoDiff(event.counter.order.senderPublicKey.toAddress)
//      )
    val submittedStatus = orderInfo(event.submitted.order.id()).status
    // worked for OHS - Buy WAVES order - filled with 2 steps, sell order - partial
    val submittedOpOrderInfoDiff = orderInfoDiffNew(
      event.submitted.order,
      updatedSubmittedInfo
    )
    // leads to -1
    // val submittedOpOrderInfoDiff = Events.createOpenPortfolio(OrderAdded(event.submittedRemaining))
    val counterInfoDiff = orderInfoDiff(event.counter.order, prevCounterInfo, updatedCounterInfo)
    val opOrderInfoDiff = Monoid.combine(
      counterInfoDiff,
      if (submittedStatus.isFinal) Map.empty[Address, OpenPortfolio] else submittedOpOrderInfoDiff
    )
    println(s"""|
                |orderExecuted:
                |
                |submittedAddedOpDiff:
                |${toString(submittedAddedOpDiff)}
                |
                |eventDiff:
                |${toString(eventDiff)}
                |
                |opDiff:
                |${toString(opDiff)}
                |
                |opOrderInfoDiff:
                |${toString(opOrderInfoDiff)}
                |
                |submittedOpOrderInfoDiff:
                |${toString(submittedOpOrderInfoDiff)}
                |
                |counterInfoDiff:
                |${toString(counterInfoDiff)}
                |
                |prevCounterInfo:
                |${toString(prevCounterInfo)}
                |
                |updatedCounterInfo:
                |${toString(updatedCounterInfo)}
                |
                |updatedSubmittedInfo:
                |${toString(updatedSubmittedInfo)}
                |
                |event.counter (id=${event.counter.order.id()}): ${event.counter}
                |event.counterRemainingAmount: ${event.counterRemainingAmount}
                |event.counterRemainingFee: ${event.counterRemainingFee}
                |
                |event.submitted (id=${event.submitted.order.id()}): ${event.submitted}
                |event.submittedRemainingAmount: ${event.submittedRemainingAmount}
                |event.submittedRemainingFee: ${event.submittedRemainingFee}
                |
                |submittedStatus: $submittedStatus
                |""".stripMargin)

    saveOpenVolume(rw, opOrderInfoDiff)

    if (!submittedStatus.isFinal) {
      import event.submitted.{order => submittedOrder}
      val k         = MatcherKeys.addressOrdersSeqNr(submittedOrder.senderPublicKey)
      val nextSeqNr = rw.get(k) + 1
      rw.put(k, nextSeqNr)
      rw.put(MatcherKeys.addressOrders(submittedOrder.senderPublicKey, nextSeqNr), Some(OrderAssets(submittedOrder.id(), event.submitted.spentAsset)))
    }
  }

  def orderCanceled(event: OrderCanceled): Unit = db.readWrite { rw =>
    saveOrderInfo(rw, event)
    saveOpenVolume(rw, Events.createOpenPortfolio(event))
  }

  def orderInfo(id: ByteStr): OrderInfo = DBUtils.orderInfo(db, id)

  def order(id: ByteStr): Option[Order] = db.get(MatcherKeys.order(id))

  def deleteOrder(address: Address, orderId: ByteStr): Boolean = db.readWrite { rw =>
    DBUtils.orderInfo(rw, orderId).status match {
      case Filled(_) | LimitOrder.Cancelled(_) =>
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
