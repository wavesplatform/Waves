package com.wavesplatform.matcher.queue

import com.google.common.primitives.Longs

case class QueueEventWithMeta(offset: QueueEventWithMeta.Offset, timestamp: Long, event: QueueEvent) {
  override def toString: String = {
    val eventStr = event match {
      case QueueEvent.Placed(o)           => s"Placed(${o.idStr()})"
      case QueueEvent.Canceled(_, id)     => s"Canceled($id)"
      case QueueEvent.OrderBookDeleted(p) => s"OrderBookDeleted(${p.key})"
    }
    s"QueueEventWithMeta(offset=$offset, ts=$timestamp, $eventStr)"
  }
}

object QueueEventWithMeta {
  type Offset = Long

  def toBytes(x: QueueEventWithMeta): Array[Byte] = Longs.toByteArray(x.offset) ++ Longs.toByteArray(x.timestamp) ++ QueueEvent.toBytes(x.event)

  def fromBytes(xs: Array[Byte]): QueueEventWithMeta = QueueEventWithMeta(
    offset = Longs.fromByteArray(xs.take(8)),
    timestamp = Longs.fromByteArray(xs.slice(8, 16)),
    event = QueueEvent.fromBytes(xs.drop(16))
  )
}
