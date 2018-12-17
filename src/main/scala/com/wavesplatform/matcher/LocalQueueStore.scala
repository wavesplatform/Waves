package com.wavesplatform.matcher

import java.util.concurrent.atomic.AtomicLong

import com.google.common.primitives.{Longs, Shorts}
import com.wavesplatform.database.{DBExt, ReadOnlyDB}
import com.wavesplatform.matcher.MatcherKeys._
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import org.iq80.leveldb.{DB, ReadOptions}

class LocalQueueStore(db: DB) {

  private val newestIdx = new AtomicLong(db.get(lqNewestIdx))

  def enqueue(event: QueueEvent, timestamp: Long): QueueEventWithMeta.Offset = {
    val idx      = newestIdx.incrementAndGet()
    val eventKey = lpqElement(idx)

    db.readWrite { rw =>
      rw.put(eventKey, Some(QueueEventWithMeta(idx, timestamp, event)))
      rw.put(lqNewestIdx, idx)
    }

    idx
  }

  def getFrom(offset: QueueEventWithMeta.Offset): Vector[QueueEventWithMeta] =
    new ReadOnlyDB(db, new ReadOptions())
      .read(LqElementKeyName, LqElementPrefixBytes, lpqElement(math.max(offset, 0)).keyBytes, Int.MaxValue) { e =>
        val offset = Longs.fromByteArray(e.getKey.slice(Shorts.BYTES, Shorts.BYTES + Longs.BYTES))
        lpqElement(offset).parse(e.getValue).getOrElse(throw new RuntimeException(s"Can't find a queue event at $offset"))
      }

}
