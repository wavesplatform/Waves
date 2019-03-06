package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.DBExt
import com.wavesplatform.matcher.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.matcher.model.{OrderInfo, OrderStatus}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

trait OrderDB {
  def containsInfo(id: ByteStr): Boolean
  def status(id: ByteStr): OrderStatus.Final
  def saveOrderInfo(id: ByteStr, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit
  def saveOrder(o: Order): Unit
  def loadRemainingOrders(owner: Address,
                          maybePair: Option[AssetPair],
                          activeOrders: Seq[(ByteStr, OrderInfo[OrderStatus])]): Seq[(ByteStr, OrderInfo[OrderStatus])]
}

object OrderDB {
  private val OldestOrderIndexOffset = 100

  def apply(settings: MatcherSettings, db: DB): OrderDB = new OrderDB with ScorexLogging {
    override def containsInfo(id: ByteStr): Boolean = db.readOnly(_.has(MatcherKeys.orderInfo(id)))

    override def status(id: ByteStr): OrderStatus.Final = db.readOnly { ro =>
      ro.get(MatcherKeys.orderInfo(id)).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status)
    }

    override def saveOrder(o: Order): Unit = db.readWrite { rw =>
      val k = MatcherKeys.order(o.id())
      if (!rw.has(k)) {
        rw.put(k, Some(o))
      }
    }

    override def saveOrderInfo(id: ByteStr, sender: Address, oi: FinalOrderInfo): Unit = {
      val orderInfoKey = MatcherKeys.orderInfo(id)
      if (db.get(orderInfoKey).isDefined) {
        log.warn(s"Finalized order info already exists for $id")
      } else {
        db.readWrite { rw =>
          val newCommonSeqNr = rw.inc(MatcherKeys.finalizedCommonSeqNr(sender))
          rw.put(MatcherKeys.finalizedCommon(sender, newCommonSeqNr), Some(id))

          val newPairSeqNr = rw.inc(MatcherKeys.finalizedPairSeqNr(sender, oi.assetPair))
          rw.put(MatcherKeys.finalizedPair(sender, oi.assetPair, newPairSeqNr), Some(id))
          if (newPairSeqNr > OldestOrderIndexOffset) // Indexes start with 1, so if newPairSeqNr=101, we delete 1 (the first)
            rw.get(MatcherKeys.finalizedPair(sender, oi.assetPair, newPairSeqNr - OldestOrderIndexOffset))
              .map(MatcherKeys.order)
              .foreach(x => rw.delete(x))

          rw.put(orderInfoKey, Some(oi))
        }
      }
    }

    override def loadRemainingOrders(owner: Address,
                                     maybePair: Option[AssetPair],
                                     activeOrders: Seq[(ByteStr, OrderInfo[OrderStatus])]): Seq[(ByteStr, OrderInfo[OrderStatus])] = db.readOnly {
      ro =>
        val (seqNr, key) = maybePair match {
          case Some(p) =>
            (ro.get(MatcherKeys.finalizedPairSeqNr(owner, p)), MatcherKeys.finalizedPair(owner, p, _: Int))
          case None =>
            (ro.get(MatcherKeys.finalizedCommonSeqNr(owner)), MatcherKeys.finalizedCommon(owner, _: Int))
        }

        activeOrders ++ (for {
          offset <- 0 until (settings.maxOrdersPerRequest - activeOrders.length)
          id     <- db.get(key(seqNr - offset))
          oi     <- db.get(MatcherKeys.orderInfo(id))
        } yield id -> oi).sorted
    }
  }

  implicit def orderInfoOrdering[S <: OrderStatus]: Ordering[(ByteStr, OrderInfo[S])] = Ordering.by { case (id, oi) => (-oi.timestamp, id) }
}
