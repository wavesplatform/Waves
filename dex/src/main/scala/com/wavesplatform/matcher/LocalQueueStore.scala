package com.wavesplatform.matcher

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicLong

import com.google.common.primitives.{Longs, Shorts}
import com.wavesplatform.database.{DBExt, ReadOnlyDB}
import com.wavesplatform.matcher.MatcherKeys._
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import org.iq80.leveldb.{DB, ReadOptions}

class LocalQueueStore(db: DB) {

  private val newestIdx        = new AtomicLong(db.get(lqNewestIdx))
  private val inMemQueue       = new ConcurrentLinkedQueue[QueueEventWithMeta]
  private var startInMemOffset = Option.empty[QueueEventWithMeta.Offset]

  def enqueue(event: QueueEvent, timestamp: Long): QueueEventWithMeta.Offset = {
    val offset   = newestIdx.incrementAndGet()
    val eventKey = lpqElement(offset)

    val x = QueueEventWithMeta(offset, timestamp, event)
    db.readWrite { rw =>
      rw.put(eventKey, Some(x))
      rw.put(lqNewestIdx, offset)
    }

    inMemQueue.add(x)
    if (startInMemOffset.isEmpty) startInMemOffset = Some(offset)
    offset
  }

  def getFrom(offset: QueueEventWithMeta.Offset, maxElements: Int): Vector[QueueEventWithMeta] = {
    if (startInMemOffset.exists(_ <= offset)) {
      if (inMemQueue.isEmpty) Vector.empty
      else {
        val xs    = Vector.newBuilder[QueueEventWithMeta]
        var added = 0

        while (!inMemQueue.isEmpty && added < maxElements) Option(inMemQueue.poll()).foreach { x =>
          xs += x
          added += 1
        }

        xs.result()
      }
    } else
      new ReadOnlyDB(db, new ReadOptions())
        .read(LqElementKeyName, LqElementPrefixBytes, lpqElement(math.max(offset, 0)).keyBytes, Int.MaxValue) { e =>
          val offset = Longs.fromByteArray(e.getKey.slice(Shorts.BYTES, Shorts.BYTES + Longs.BYTES))
          lpqElement(offset).parse(e.getValue).getOrElse(throw new RuntimeException(s"Can't find a queue event at $offset"))
        }
  }

  def newestOffset: Option[QueueEventWithMeta.Offset] = {
    val idx      = newestIdx.get()
    val eventKey = lpqElement(idx)
    eventKey.parse(db.get(eventKey.keyBytes)).map(_.offset)
  }

  def dropUntil(offset: QueueEventWithMeta.Offset): Unit = db.readWrite { rw =>
    val oldestIdx = math.max(db.get(lqOldestIdx), 0)
    (oldestIdx until offset).foreach { offset =>
      rw.delete(lpqElement(offset))
    }
    rw.put(lqOldestIdx, offset)
  }

}
