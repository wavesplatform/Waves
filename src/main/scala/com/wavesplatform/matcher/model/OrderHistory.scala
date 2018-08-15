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
  import OrderHistory._
  import com.wavesplatform.matcher.MatcherKeys._

  private val timer               = Kamon.timer("matcher.order-history.impl")
  private val saveOpenVolumeTimer = timer.refine("action" -> "save-open-volume")
  private val saveOrderInfoTimer  = timer.refine("action" -> "save-order-info")
  private val openVolumeTimer     = timer.refine("action" -> "open-volume")

  private def combine(order: Order, curr: Option[OrderInfo], diff: OrderInfoDiff): OrderInfo = {
    val r = curr match {
      case None =>
        val executedAmount = diff.addExecutedAmount.getOrElse(0L)
        val remainingFee   = order.matcherFee - diff.executedFee.getOrElse(0L)
        OrderInfo(
          amount = order.amount,
          filled = executedAmount,
          canceled = diff.nowCanceled.getOrElse(false),
          minAmount = diff.newMinAmount,
          remainingFee = remainingFee,
          unsafeTotalSpend = diff.lastSpend.orElse(Some(0L))
        )
      case Some(x) =>
        OrderInfo(
          amount = order.amount,
          filled = x.filled + diff.addExecutedAmount.getOrElse(0L),
          canceled = diff.nowCanceled.getOrElse(x.canceled),
          minAmount = diff.newMinAmount.orElse(x.minAmount),
          remainingFee = x.remainingFee - diff.executedFee.getOrElse(0L),
          unsafeTotalSpend = Some(OrderInfo.safeSum(x.totalSpend(LimitOrder(order)), diff.lastSpend.getOrElse(0L)))
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

  private def saveOrderInfo(rw: RW, event: Event): Map[ByteStr, OrderInfoChange] =
    saveOrderInfoTimer.measure(db.readWrite { rw =>
      val diff = Events.collectChanges(event)

      println(s"""
           |saveOrderInfo: ${diff.size} changes
           |""".stripMargin)
      val updatedInfo = diff.map {
        case (o, d) =>
          val orderId  = o.id()
          val curr     = DBUtils.orderInfoOpt(rw, orderId)
          val combined = combine(o, curr, d)
          println(s"Saving new order info for $orderId: $combined")
          rw.put(MatcherKeys.orderInfo(orderId), combined)
          o.id() -> OrderInfoChange(o, curr, combined)
      }

      updatedInfo.toMap
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
    println(s"orderAccepted start: $event")
    val lo = event.order
    saveOrder(rw, lo.order)

    // OrderAccepted sent after matchOrder!
    println(s"orderAccepted for ${event.order.order.id()} prev order info: ${orderInfo(lo.order.id())}")
    val updated = saveOrderInfo(rw, event)

    if (updated(lo.order.id()).isNew) {
      val opDiff = diffAccepted(updated(lo.order.id()))
      println(s"""|
                |orderAccepted:
                |opDiff:
                |${toString(opDiff)}
                |
                |""".stripMargin)
      saveOpenVolume(rw, opDiff)

      // for OrderAdded events, updatedInfo contains just one element
      for (x <- updated.values) {
        val o         = x.order
        val k         = MatcherKeys.addressOrdersSeqNr(o.senderPublicKey)
        val nextSeqNr = rw.get(k) + 1
        rw.put(k, nextSeqNr)

        val spendAssetId = if (o.orderType == OrderType.BUY) o.assetPair.priceAsset else o.assetPair.amountAsset
        rw.put(MatcherKeys.addressOrders(o.senderPublicKey, nextSeqNr), Some(OrderAssets(o.id(), spendAssetId)))
      }
    }

    println(s"orderAccepted end: $event")
  }

  def toString(x: OrderInfo): String = {
    s"""|$x
        |remaining: ${x.remaining}
        |remainingFee: ${x.remainingFee}""".stripMargin
  }

  def orderExecuted(event: OrderExecuted): Unit = db.readWrite { rw =>
    println(s"orderExecuted start: $event")
    saveOrder(rw, event.submitted.order)

    val updated   = saveOrderInfo(rw, event)
    val submitted = updated(event.submitted.order.id())

    val submittedOpOrderInfoDiff = diffAccepted(updated(event.submitted.order.id()))
    val counterInfoDiff          = diffExecuted(updated(event.counter.order.id()))
    val opOrderInfoDiff = Monoid.combine(
      counterInfoDiff,
      if (submitted.updatedInfo.status.isFinal) Map.empty[Address, OpenPortfolio] else submittedOpOrderInfoDiff
    )
    println(s"""|
                |orderExecuted:
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
                |event.counter (id=${event.counter.order.id()}): ${event.counter}
                |event.counterRemainingAmount: ${event.counterRemainingAmount}
                |event.counterRemainingFee: ${event.counterRemainingFee}
                |
                |event.submitted (id=${event.submitted.order.id()}): ${event.submitted}
                |event.submittedRemainingAmount: ${event.submittedRemainingAmount}
                |event.submittedRemainingFee: ${event.submittedRemainingFee}
                |""".stripMargin)

    saveOpenVolume(rw, opOrderInfoDiff)

    println(
      s"Adding ${submitted.order.id()} to ${submitted.order.senderPublicKey}? isFinal: ${submitted.updatedInfo.status.isFinal}, isNew: ${submitted.isNew}")
    if (submitted.isNew) {
      import event.submitted.{order => submittedOrder}
      println(s"Adding ${submitted.order.id()} to ${submittedOrder.senderPublicKey}")
      val k         = MatcherKeys.addressOrdersSeqNr(submittedOrder.senderPublicKey)
      val nextSeqNr = rw.get(k) + 1
      rw.put(k, nextSeqNr)
      rw.put(
        MatcherKeys.addressOrders(submittedOrder.senderPublicKey.toAddress, nextSeqNr),
        Some(OrderAssets(submittedOrder.id(), event.submitted.spentAsset))
      )
    }
    println(s"orderExecuted end: $event")
  }

  def orderCanceled(event: OrderCanceled): Unit = db.readWrite { rw =>
    println(s"orderCanceled start: $event")
    val updated = saveOrderInfo(rw, event)
    val opDiff  = diffCancel(updated(event.limitOrder.order.id()))
    println(s"""|
          |info: $updated
          |opDiff: $opDiff
          |""".stripMargin)
    saveOpenVolume(rw, opDiff)
    println(s"orderCanceled end: $event")
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

  case class OrderInfoChange(order: Order, origInfo: Option[OrderInfo], updatedInfo: OrderInfo) {
    def isNew: Boolean = origInfo.isEmpty
  }

  object OrderHistoryOrdering extends Ordering[(ByteStr, OrderInfo, Option[Order])] {
    def orderBy(oh: (ByteStr, OrderInfo, Option[Order])): (OrderStatus, Long) = (oh._2.status, -oh._3.map(_.timestamp).getOrElse(0L))

    override def compare(first: (ByteStr, OrderInfo, Option[Order]), second: (ByteStr, OrderInfo, Option[Order])): Int = {
      implicitly[Ordering[(OrderStatus, Long)]].compare(orderBy(first), orderBy(second))
    }
  }

  def diffAccepted(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change._
    val lo             = LimitOrder(order)
    val maxSpendAmount = lo.getRawSpendAmount
    val remainingSpend = maxSpendAmount - updatedInfo.totalSpend(lo)
    val remainingFee   = if (lo.feeAcc == lo.rcvAcc) math.max(updatedInfo.remainingFee - lo.getReceiveAmount, 0L) else updatedInfo.remainingFee

    println(s"orderInfoDiffNew: remaining spend=$remainingSpend, remaining fee=$remainingFee")
    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> remainingSpend),
          Map(lo.feeAsset           -> remainingFee)
        )
      )
    )
  }

  def diffExecuted(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change.{order, updatedInfo}
    val prev         = change.origInfo.getOrElse(throw new IllegalStateException("origInfo must be defined"))
    val lo           = LimitOrder(order)
    val changedSpend = prev.totalSpend(lo) - updatedInfo.totalSpend(lo)
    val changedFee   = -releaseFee(order, prev.remainingFee, updatedInfo.remainingFee)
    println(s"orderInfoDiff: changed spend=$changedSpend, fee=$changedFee")
    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> changedSpend),
          Map(lo.feeAsset           -> changedFee)
        )
      )
    )
  }

  def diffCancel(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change.{order, updatedInfo}
    val lo             = LimitOrder(order)
    val maxSpendAmount = lo.getRawSpendAmount
    val remainingSpend = updatedInfo.totalSpend(lo) - maxSpendAmount
    val remainingFee   = -releaseFee(order, updatedInfo.remainingFee, 0)

    println(s"orderInfoDiffCancel: remaining spend=$remainingSpend, remaining fee=$remainingFee")
    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> remainingSpend),
          Map(lo.feeAsset           -> remainingFee)
        )
      )
    )
  }

  private def releaseFee(totalReceiveAmount: Long, matcherFee: Long, prevRemaining: Long, updatedRemaining: Long): Long = {
    val executedBefore = matcherFee - prevRemaining
    val restReserved   = math.max(matcherFee - totalReceiveAmount - executedBefore, 0L)

    val executed = prevRemaining - updatedRemaining
    println(s"""|
            |releaseFee:
                |totalReceiveAmount: $totalReceiveAmount
                |matcherFee: $matcherFee
                |prevRemaining: $prevRemaining
                |executedBefore: $executedBefore
                |restReserved: $restReserved
                |executed: $executed
                |""".stripMargin)
    math.min(executed, restReserved)
  }

  private def releaseFee(order: Order, prevRemaining: Long, updatedRemaining: Long): Long = {
    val lo = LimitOrder(order)
    if (lo.rcvAsset == lo.feeAsset) releaseFee(lo.getReceiveAmount, order.matcherFee, prevRemaining, updatedRemaining)
    else prevRemaining - updatedRemaining
  }

}
