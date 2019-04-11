package com.wavesplatform.matcher.util

import java.util.concurrent.ConcurrentSkipListMap

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction

class TrackedBroadcastTransactions {
  private type Key = (Long, ByteStr) // Two transactions can have same timestamp
  private val storage = new ConcurrentSkipListMap[Key, ExchangeTransaction](Ordering[Key])

  def add(sendMs: Long, tx: ExchangeTransaction): Unit = storage.put(key(sendMs, tx), tx)
  def refresh(prevSendMs: Long, nextSendMs: Long, tx: ExchangeTransaction): Unit = {
    storage.remove(key(prevSendMs, tx))
    add(nextSendMs, tx)
  }

  def remove(lastSendMs: Long, tx: ExchangeTransaction): Unit = storage.remove(key(lastSendMs, tx))

  def ready(fromMs: Long): Iterator[(Long, ExchangeTransaction)] = {
    import scala.collection.JavaConverters._
    val startKey = (fromMs, ByteStr.empty)
    storage
      .tailMap(startKey)
      .asScala
      .iterator // here, because lastSendMs could be same for multiple transactions
      .map {
      case ((lastSendMs, _), tx) => (lastSendMs, tx)
    }
  }

  private def key(ms: Long, tx: ExchangeTransaction): Key = (ms, tx.id())
}
