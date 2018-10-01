package com.wavesplatform.matcher.model

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.database.{DBExt, RW}
import com.wavesplatform.matcher._
import com.wavesplatform.matcher.api.DBUtils
import com.wavesplatform.matcher.model.Events._
import com.wavesplatform.matcher.model.LimitOrder._
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.state._
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.utils.ScorexLogging
import kamon.Kamon
import org.iq80.leveldb.DB

class OrderHistory(db: DB, settings: MatcherSettings) extends ScorexLogging {
  import OrderHistory._
  import com.wavesplatform.matcher.MatcherKeys._

  private val timer               = Kamon.timer("matcher.order-history.impl")
  private val saveOpenVolumeTimer = timer.refine("action" -> "save-open-volume")
  private val saveOrderInfoTimer  = timer.refine("action" -> "save-order-info")
  private val openVolumeTimer     = timer.refine("action" -> "open-volume")

  private def combine(order: Order, curr: Option[OrderInfo], diff: OrderInfoDiff, event: Event): OrderInfo = curr match {
    case Some(x) =>
      OrderInfo(
        amount = order.amount,
        filled = x.filled + diff.addExecutedAmount.getOrElse(0L),
        canceledByUser = diff.cancelledByUser.orElse(x.canceledByUser),
        minAmount = diff.newMinAmount.orElse(x.minAmount),
        remainingFee = x.remainingFee - diff.executedFee.getOrElse(0L),
        unsafeTotalSpend = Some(OrderInfo.safeSum(x.totalSpend(LimitOrder(order)), diff.lastSpend.getOrElse(0L)))
      )
    case None =>
      val executedAmount = diff.addExecutedAmount.getOrElse(0L)
      val remainingFee   = order.matcherFee - diff.executedFee.getOrElse(0L)

      OrderInfo(
        amount = order.amount,
        filled = executedAmount,
        canceledByUser = diff.cancelledByUser,
        minAmount = diff.newMinAmount,
        remainingFee = remainingFee,
        unsafeTotalSpend = diff.lastSpend.orElse(Some(0L))
      )
  }

  private def saveOrderInfo(rw: RW, event: Event): Unit = saveOrderInfoTimer.measure {
    val orderInfoDiffs = collectChanges(event)

    val changes = orderInfoDiffs.foldLeft(Map.empty[Order.Id, OrderInfoChange]) {
      case (origChanges, (order, orderInfoDiff)) =>
        val orderId  = order.id()
        val origInfo = rw.get(MatcherKeys.orderInfoOpt(orderId))
        if (origInfo.exists(_.status.isFinal)) origChanges
        else {
          val combinedInfo = combine(order, origInfo, orderInfoDiff, event)
          val change       = OrderInfoChange(order, origInfo, combinedInfo)

          log.trace(s"$orderId: ${change.origInfo.fold("[]")(_.status.toString)} -> ${change.updatedInfo.status}")
          origChanges.updated(orderId, change)
        }
    }

    changes.foreach {
      case (id, change) =>
        rw.put(MatcherKeys.orderInfo(id), change.updatedInfo)

        if (change.origInfo.isEmpty) {
          saveOrder(rw, change.order)
        }
        updateOrderIndexes(rw, change)
    }

    val opDiff = diff(changes)
    saveOpenVolume(rw, opDiff)
  }

  private def collectChanges(event: Event): Seq[(Order, OrderInfoDiff)] = event match {
    case OrderAdded(lo) =>
      Seq((lo.order, OrderInfoDiff(newMinAmount = Some(lo.minAmountOfAmountAsset))))

    case oe: OrderExecuted =>
      val submitted = oe.submittedExecuted
      val counter   = oe.counterExecuted

      Seq(
        (submitted.order,
         OrderInfoDiff(
           addExecutedAmount = Some(oe.executedAmount),
           executedFee = Some(submitted.fee),
           newMinAmount = Some(submitted.minAmountOfAmountAsset),
           lastSpend = Some(submitted.getSpendAmount)
         )),
        (counter.order,
         OrderInfoDiff(
           addExecutedAmount = Some(oe.executedAmount),
           executedFee = Some(counter.fee),
           newMinAmount = Some(counter.minAmountOfAmountAsset),
           lastSpend = Some(counter.getSpendAmount)
         ))
      )

    case OrderCanceled(lo, unmatchable) =>
      Seq((lo.order, OrderInfoDiff(cancelledByUser = Some(!unmatchable))))
  }

  def openVolume(address: Address, assetId: Option[AssetId]): Long =
    openVolumeTimer.measure(db.get(MatcherKeys.openVolume(address, assetId)).getOrElse(0L))

  private def saveOpenVolume(rw: RW, opDiff: Map[Address, OpenPortfolio]): Unit = saveOpenVolumeTimer.measure {
    for ((address, op) <- opDiff) {
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

  private def updateOrderIndexes(rw: RW, change: OrderInfoChange): Unit = {
    import change.{order => o}
    val address = o.senderPublicKey.toAddress
    val id      = o.id()

    // key + stale read issue
    val activeOrdersIndex    = new ActiveOrdersIndex(address, 200)
    val nonActiveCommonIndex = new NonActiveOrdersCommonIndex(address, 100)
    val nonActivePairIndex   = new NonActiveOrdersPairIndex(address, o.assetPair, 100)

    if (change.updatedInfo.status.isFinal) {
      if (change.origInfo.nonEmpty) activeOrdersIndex.delete(rw, id)
      nonActiveCommonIndex.add(rw, id)
      nonActivePairIndex.add(rw, id)
    } else if (change.origInfo.isEmpty) activeOrdersIndex.add(rw, o.assetPair, id)
  }

  def process(event: Event): Unit       = db.readWrite(saveOrderInfo(_, event))
  def orderInfo(id: ByteStr): OrderInfo = DBUtils.orderInfo(db, id)
  def order(id: ByteStr): Option[Order] = DBUtils.order(db, id)

  def deleteOrder(address: Address, orderId: ByteStr): Either[OrderStatus, Unit] = db.readWrite { rw =>
    DBUtils.orderInfo(rw, orderId).status match {
      case Filled(_) | LimitOrder.Cancelled(_) =>
        rw.delete(MatcherKeys.order(orderId))
        rw.delete(MatcherKeys.orderInfo(orderId))
        Right(())
      case nonFinalStatus => Left(nonFinalStatus)
    }
  }
}

object OrderHistory extends ScorexLogging {
  import OrderInfo.orderStatusOrdering

  case class OrderInfoChange(order: Order, origInfo: Option[OrderInfo], updatedInfo: OrderInfo)

  object OrderHistoryOrdering extends Ordering[(ByteStr, OrderInfo, Option[Order])] {
    def orderBy(oh: (ByteStr, OrderInfo, Option[Order])): (OrderStatus, Long) = (oh._2.status, -oh._3.map(_.timestamp).getOrElse(0L))

    override def compare(first: (ByteStr, OrderInfo, Option[Order]), second: (ByteStr, OrderInfo, Option[Order])): Int = {
      implicitly[Ordering[(OrderStatus, Long)]].compare(orderBy(first), orderBy(second))
    }
  }

  private case class OrderInfoDiff(addExecutedAmount: Option[Long] = None,
                                   cancelledByUser: Option[Boolean] = None,
                                   newMinAmount: Option[Long] = None,
                                   executedFee: Option[Long] = None,
                                   lastSpend: Option[Long] = None) {
    override def toString: String =
      s"OrderInfoDiff(${addExecutedAmount.fold("")(x => s"addExecutedAmount=$x, ")}" +
        cancelledByUser.fold("")(x => s"cancelledByUser=$x, ") +
        newMinAmount.fold("")(x => s"newMinAmount=$x, ") +
        executedFee.fold("")(x => s"executedFee=$x, ") +
        lastSpend.fold(")")(x => s"lastSpend=$x)")
  }

  def diff(changes: Map[ByteStr, OrderInfoChange]): Map[Address, OpenPortfolio] = {
    changes.values.foldLeft(Map.empty[Address, OpenPortfolio]) {
      case (r, change) =>
        Monoid.combine(
          r,
          change.origInfo match {
            case Some(origInfo) if origInfo.status.isFinal =>
              log.warn(
                s"Trying to create a diff for a finalized order '${change.order.id()}', origInfo: $origInfo, updatedInfo: ${change.updatedInfo}"
              )
              Map.empty
            case Some(_) => if (change.updatedInfo.status.isFinal) diffRelease(change) else diffUpdate(change)
            case None    => if (change.updatedInfo.status.isFinal) Map.empty else diffNew(change)
          }
        )
    }
  }

  private def diffNew(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change.{order, updatedInfo}
    val lo             = LimitOrder(order)
    val maxSpendAmount = lo.getRawSpendAmount
    diffMap(
      lo,
      diffSpend = maxSpendAmount - updatedInfo.totalSpend(lo),
      diffFee = releaseFee(order, change.updatedInfo.remainingFee, updatedRemaining = 0)
    )
  }

  private def diffUpdate(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change.{order, updatedInfo}
    val prev = change.origInfo.getOrElse(throw new IllegalStateException("origInfo must be defined"))
    val lo   = LimitOrder(order)
    diffMap(
      lo,
      diffSpend = prev.totalSpend(lo) - updatedInfo.totalSpend(lo),
      diffFee = -releaseFee(order, prev.remainingFee, updatedInfo.remainingFee)
    )
  }

  private def diffRelease(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
    import change.order
    val lo             = LimitOrder(order)
    val prev           = change.origInfo.getOrElse(throw new IllegalStateException("origInfo must be defined"))
    val maxSpendAmount = lo.getRawSpendAmount
    diffMap(
      lo,
      diffSpend = prev.totalSpend(lo) - maxSpendAmount,
      diffFee = -releaseFee(order, prev.remainingFee, updatedRemaining = 0)
    )
  }

  private def diffMap(lo: LimitOrder, diffSpend: Long, diffFee: Long): Map[Address, OpenPortfolio] = {
    import lo.order
    Map(
      order.sender.toAddress -> OpenPortfolio(
        Monoid.combine(
          Map(order.getSpendAssetId -> diffSpend),
          Map(lo.feeAsset           -> diffFee)
        )
      )
    )
  }

  /**
    * @return How much reserved fee we should return during this update
    */
  private def releaseFee(totalReceiveAmount: Long, matcherFee: Long, prevRemaining: Long, updatedRemaining: Long): Long = {
    val executedBefore = matcherFee - prevRemaining
    val restReserved   = math.max(matcherFee - totalReceiveAmount - executedBefore, 0L)

    val executed = prevRemaining - updatedRemaining
    math.min(executed, restReserved)
  }

  private def releaseFee(order: Order, prevRemaining: Long, updatedRemaining: Long): Long = {
    val lo = LimitOrder(order)
    if (lo.rcvAsset == lo.feeAsset) releaseFee(lo.getReceiveAmount, order.matcherFee, prevRemaining, updatedRemaining)
    else prevRemaining - updatedRemaining
  }

}
