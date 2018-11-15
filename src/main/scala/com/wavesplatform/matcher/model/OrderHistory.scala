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
  import DBUtils.indexes
  import OrderHistory._
  import com.wavesplatform.matcher.MatcherKeys._

  private val timer               = Kamon.timer("matcher.order-history.impl")
  private val saveOpenVolumeTimer = timer.refine("action" -> "save-open-volume")
  private val saveOrderInfoTimer  = timer.refine("action" -> "save-order-info")

  private def updateTimestamp(rw: RW, address: Address, newTimestamp: Long): Either[String, Long] = {
    val k = lastCommandTimestamp(address)
    rw.get(k) match {
      case None =>
        rw.put(k, Some(newTimestamp))
        Right(newTimestamp)
      case Some(prevTimestamp) =>
        val earliestTimestamp = prevTimestamp - settings.orderTimestampDrift
        if (newTimestamp < earliestTimestamp) {
          Left(s"Timestamp must be >= $earliestTimestamp")
        } else if (newTimestamp <= prevTimestamp) {
          Right(prevTimestamp)
        } else {
          rw.put(k, Some(newTimestamp))
          Right(newTimestamp)
        }
    }
  }

  def updateTimestamp(address: Address, newTimestamp: Long): Either[String, Long] = db.readWrite(rw => updateTimestamp(rw, address, newTimestamp))

  def process(event: OrderAdded): Unit = saveOrderInfoTimer.measure {
    db.readWrite { rw =>
      val order = event.order.order

      updateTimestamp(rw, order.sender, order.timestamp)

      val origInfo = rw.get(orderInfoOpt(order.id()))
      if (!origInfo.exists(_.status.isFinal)) {
        val change = OrderInfoChange(
          order = order,
          origInfo = origInfo,
          updatedInfo = origInfo.getOrElse(OrderInfo.emptyFor(order)).copy(minAmount = Some(event.order.minAmountOfAmountAsset))
        )

        if (change.hasChanges) {
          log.trace(s"${order.id()}: ${change.origInfo.fold("[]")(_.status.toString)} -> ${change.updatedInfo.status}")
          val d = diff(List(change))
          saveOpenVolume(rw, d)

          if (change.origInfo.isEmpty) rw.put(MatcherKeys.order(order.id()), Some(order))
          rw.put(MatcherKeys.orderInfo(order.id()), change.updatedInfo)
        }

        // Hack until DEX-160 is done
        // We need to not add the same orders to index during recovery
        // Recovery happens from a snapshot + commands
        val senderAddress = order.senderPublicKey.toAddress
        if (!indexes.active.has(rw, senderAddress, order.id())) indexes.active.add(rw, senderAddress, order.assetPair, order.id())
      }
    }
  }

  def process(event: OrderExecuted): Unit = saveOrderInfoTimer.measure {
    db.readWrite { rw =>
      def origInfo(lo: LimitOrder): Option[OrderInfo] = rw.get(orderInfoOpt(lo.order.id()))
      def wasActive(x: Option[OrderInfo]): Boolean    = !x.exists(_.status.isFinal)

      lazy val counterOrigInfo   = origInfo(event.counterExecuted)
      lazy val submittedOrigInfo = origInfo(event.submittedExecuted)

      if (wasActive(counterOrigInfo) && wasActive(submittedOrigInfo)) {
        val counterChanges   = collectChanges(event, event.counterExecuted, counterOrigInfo)
        val submittedChanges = collectChanges(event, event.submittedExecuted, submittedOrigInfo)
        val allChanges       = List(counterChanges, submittedChanges)
        if (allChanges.forall(_.hasChanges)) {
          val mergedDiff = diff(List(counterChanges, submittedChanges))
          saveOpenVolume(rw, mergedDiff)

          if (submittedChanges.origInfo.isEmpty) rw.put(MatcherKeys.order(submittedChanges.order.id()), Some(submittedChanges.order))
          allChanges.foreach { change =>
            log.trace(s"${change.order.id()}: ${change.origInfo.fold("[]")(_.status.toString)} -> ${change.updatedInfo.status}")
            rw.put(MatcherKeys.orderInfo(change.order.id()), change.updatedInfo)
          }

          import event.counter.{order => counterOrder}
          if (counterChanges.updatedInfo.status.isFinal) indexes.active.delete(rw, counterOrder.senderPublicKey.toAddress, counterOrder.id())

          allChanges
            .filter(_.updatedInfo.status.isFinal)
            .groupBy(_.order.senderPublicKey.toAddress)
            .foreach {
              case (address, changes) =>
                val ids = changes.map(_.order.id())
                indexes.finalized.common.add(rw, address, ids)
                indexes.finalized.pair.add(rw, address, counterOrder.assetPair, ids)
            }
        }
      }
    }
  }

  def process(event: OrderCanceled): Unit = saveOrderInfoTimer.measure {
    db.readWrite { rw =>
      val order    = event.limitOrder.order
      val origInfo = rw.get(orderInfoOpt(order.id()))
      if (!origInfo.exists(_.status.isFinal)) {
        val change = OrderInfoChange(
          order = order,
          origInfo = origInfo,
          updatedInfo = origInfo.getOrElse(OrderInfo.emptyFor(order)).copy(canceledByUser = Some(!event.unmatchable))
        )

        if (change.hasChanges) {
          log.trace(s"${order.id()}: ${change.origInfo.fold("[]")(_.status.toString)} -> ${change.updatedInfo.status}")
          saveOpenVolume(rw, diff(List(change)))

          rw.put(MatcherKeys.orderInfo(order.id()), change.updatedInfo)
          if (change.origInfo.isEmpty) rw.put(MatcherKeys.order(order.id()), Some(order))

          val address = order.senderPublicKey.toAddress
          indexes.active.delete(rw, address, order.id())
          indexes.finalized.common.add(rw, address, order.id())
          indexes.finalized.pair.add(rw, address, order.assetPair, order.id())
        }
      }
    }
  }

  private def collectChanges(event: OrderExecuted, loExecuted: LimitOrder, origInfoOpt: Option[OrderInfo]): OrderInfoChange = {
    val origInfo = origInfoOpt.getOrElse(OrderInfo.emptyFor(loExecuted.order))
    OrderInfoChange(
      order = loExecuted.order,
      origInfo = origInfoOpt,
      updatedInfo = origInfo.copy(
        filled = origInfo.filled + event.executedAmount,
        minAmount = Some(loExecuted.minAmountOfAmountAsset),
        remainingFee = origInfo.remainingFee - loExecuted.fee,
        unsafeTotalSpend = Some(OrderInfo.safeSum(origInfo.totalSpend(LimitOrder(loExecuted.order)), loExecuted.getSpendAmount))
      )
    )
  }

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

  def orderInfo(id: Order.Id): OrderInfo = DBUtils.orderInfo(db, id)
  def order(id: Order.Id): Option[Order] = DBUtils.order(db, id)

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

  case class OrderInfoChange(order: Order, origInfo: Option[OrderInfo], updatedInfo: OrderInfo) {
    def hasChanges: Boolean = !origInfo.contains(updatedInfo)
  }

  object OrderHistoryOrdering extends Ordering[(ByteStr, OrderInfo, Option[Order])] {
    def orderBy(oh: (ByteStr, OrderInfo, Option[Order])): (OrderStatus, Long) = (oh._2.status, -oh._3.map(_.timestamp).getOrElse(0L))

    override def compare(first: (ByteStr, OrderInfo, Option[Order]), second: (ByteStr, OrderInfo, Option[Order])): Int = {
      implicitly[Ordering[(OrderStatus, Long)]].compare(orderBy(first), orderBy(second))
    }
  }

  def diff(changes: List[OrderInfoChange]): Map[Address, OpenPortfolio] = {
    changes.foldLeft(Map.empty[Address, OpenPortfolio]) {
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
