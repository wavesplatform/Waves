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

  private def saveOrderInfo(rw: RW, event: OrderAdded): Unit = saveOrderInfoTimer.measure {
    val order    = event.order.order
    val orderId  = order.id()
    val origInfo = rw.get(MatcherKeys.orderInfoOpt(orderId))
    println(s"saveOrderInfo: added: origInfo: $origInfo")
    val change = OrderInfoChange(
      order = order,
      origInfo = origInfo,
      updatedInfo = origInfo.getOrElse(OrderInfo.emptyFor(order)).copy(minAmount = Some(event.order.minAmountOfAmountAsset))
    )

    log.trace(s"$orderId: ${change.origInfo.fold("[]")(_.status.toString)} -> ${change.updatedInfo.status}")

    val can = !origInfo.exists(_.status.isFinal)
    println(s"saveOrderInfo: added: can: $can (order.id=${change.order.id()})")
    if (can) {
      saveOpenVolume(rw, diff(List(change)))

      rw.put(MatcherKeys.orderInfo(order.id()), change.updatedInfo)
      if (change.origInfo.isEmpty) saveOrder(rw, change.order)

      //if (can) {
      def activeIndex = new ActiveOrdersIndex(order.senderPublicKey.toAddress, 200)
      activeIndex.add(rw, order.assetPair, orderId)
      //}
    }
  }

  private def saveOrderInfo(rw: RW, event: OrderExecuted): Unit = {
    def changesFor(lo: LimitOrder): OrderInfoChange = {
      val origInfo = rw.get(MatcherKeys.orderInfoOpt(lo.order.id()))
      collectChanges(event, lo, origInfo)
    }

    def getOpenVolume(change: OrderInfoChange): Map[Address, OpenPortfolio] = {
      log.trace(s"${change.order.id()}: ${change.origInfo.fold("[]")(_.status.toString)} -> ${change.updatedInfo.status}")

      /*if (change.origInfo.exists(_.status.isFinal)) Map.empty
      else */
      diff(List(change))
    }

    saveOrderInfoTimer.measure {
      import event.counter.{order => counterOrder}

      val counterChanges = changesFor(event.counterExecuted)
      println(s"saveOrderInfo: executed: counterChanges=$counterChanges")
      val submittedChanges = changesFor(event.submittedExecuted)
      println(s"saveOrderInfo: executed: submittedChanges=$submittedChanges")

      if (submittedChanges.origInfo.isEmpty) rw.put(MatcherKeys.order(submittedChanges.order.id()), Some(submittedChanges.order))

      val allChanges = List(counterChanges, submittedChanges)

      val canCounter   = !counterChanges.origInfo.exists(_.status.isFinal)
      val canSubmitted = !submittedChanges.origInfo.exists(_.status.isFinal)
      println(
        s"saveOrderInfo: executed: canCounter=$canCounter (order.id=${counterChanges.order.id()}), " +
          s"canSubmitted=$canSubmitted (order.id=${submittedChanges.order.id()})"
      )
      if (canCounter && canSubmitted) {
        val mergedDiff = Monoid.combine(getOpenVolume(counterChanges), getOpenVolume(submittedChanges))
        saveOpenVolume(rw, mergedDiff)

        allChanges.foreach { change =>
          rw.put(MatcherKeys.orderInfo(change.order.id()), change.updatedInfo) // todo move
        }

//        if (canCounter) {
        def activeCounterIndex = new ActiveOrdersIndex(counterOrder.senderPublicKey.toAddress, 200)
        if (counterChanges.updatedInfo.status.isFinal) activeCounterIndex.delete(rw, counterOrder.id())
//        }

        allChanges
//          .filter { x =>
//            x == counterChanges && canCounter || x == submittedChanges && canSubmitted
//          }
          .filter(_.updatedInfo.status.isFinal)
          .groupBy(_.order.senderPublicKey.toAddress)
          .foreach {
            case (address, changes) =>
              val ids = changes.map(_.order.id())
              println(s"add to finalized index: $address -> ${ids.mkString(", ")}")
              new FinalizedOrdersCommonIndex(address, 100).add(rw, ids: _*)
              new FinalizedOrdersPairIndex(address, counterOrder.assetPair, 100).add(rw, ids: _*)
          }
      }
    }
  }

  private def saveOrderInfo(rw: RW, event: OrderCanceled): Unit = saveOrderInfoTimer.measure {
    val order    = event.limitOrder.order
    val orderId  = order.id()
    val origInfo = rw.get(MatcherKeys.orderInfoOpt(orderId))
    val change = OrderInfoChange(
      order = order,
      origInfo = origInfo,
      updatedInfo = origInfo.getOrElse(OrderInfo.emptyFor(order)).copy(canceledByUser = Some(!event.unmatchable))
    )

    log.trace(s"$orderId: ${change.origInfo.fold("[]")(_.status.toString)} -> ${change.updatedInfo.status}")

    val can = !origInfo.exists(_.status.isFinal)
    println(s"saveOrderInfo: canceled: can: $can (order.id=${change.order.id()})")
    if (can) {
      saveOpenVolume(rw, diff(List(change)))

      rw.put(MatcherKeys.orderInfo(order.id()), change.updatedInfo)
      if (change.origInfo.isEmpty) saveOrder(rw, change.order)

      def activeIndex          = new ActiveOrdersIndex(order.senderPublicKey.toAddress, 200)
      def finalizedCommonIndex = new FinalizedOrdersCommonIndex(order.senderPublicKey.toAddress, 100)
      def finalizedPairIndex   = new FinalizedOrdersPairIndex(order.senderPublicKey.toAddress, order.assetPair, 100)

      println(s"saveOrderInfo: canceled: delete $orderId")

      //if (can) {
      activeIndex.delete(rw, orderId)

      // TODO: cancel, add, cancel
      finalizedCommonIndex.add(rw, orderId)
      finalizedPairIndex.add(rw, orderId)
      //}
    }
  }

  private def collectChanges(event: OrderExecuted, loExecuted: LimitOrder, origInfoOpt: Option[OrderInfo]): OrderInfoChange = {
    val origInfo = origInfoOpt.getOrElse(OrderInfo.emptyFor(loExecuted.order))
    println(
      s"collectChanges: executed: loExecuted.order.id=${loExecuted.order.id()}, loExecuted.fee=${loExecuted.fee}, origInfo.remainingFee=${origInfo.remainingFee}")
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

  def process(event: OrderAdded): Unit    = db.readWrite(saveOrderInfo(_, event))
  def process(event: OrderExecuted): Unit = db.readWrite(saveOrderInfo(_, event))
  def process(event: OrderCanceled): Unit = db.readWrite(saveOrderInfo(_, event))

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

  case class IndexChanges(toAdd: Seq[Order], toDelete: Seq[Order]) {
    def add(x: Order): IndexChanges    = ???
    def delete(x: Order): IndexChanges = ???
  }

  object IndexChanges {
    val empty = IndexChanges(Seq.empty, Seq.empty)
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
