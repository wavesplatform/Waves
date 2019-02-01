package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.database.DBExt
import com.wavesplatform.matcher.model.OrderInfo.FinalOrderInfo
import com.wavesplatform.matcher.model.{OrderInfo, OrderStatus}
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

trait OrderDB {
  def contains(id: ByteStr): Boolean
  def status(id: ByteStr): OrderStatus.Final
  def saveOrderInfo(id: ByteStr, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit
  def saveOrder(o: Order): Unit
  def loadRemainingOrders(owner: Address,
                          maybePair: Option[AssetPair],
                          activeOrders: Seq[(ByteStr, OrderInfo[OrderStatus])]): Seq[(ByteStr, OrderInfo[OrderStatus])]
}

object OrderDB {
  def apply(settings: MatcherSettings, db: DB): OrderDB = new OrderDB with ScorexLogging {
    override def contains(id: ByteStr): Boolean = db.readOnly(_.has(MatcherKeys.order(id)))

    override def status(id: ByteStr): OrderStatus.Final = db.readOnly { ro =>
      ro.get(MatcherKeys.orderInfo(id)).fold[OrderStatus.Final](OrderStatus.NotFound)(_.status)
    }

    override def saveOrder(o: Order): Unit = {
      db.readWrite(_.put(MatcherKeys.order(o.id()), Some(o)))
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
