package com.wavesplatform.database

import java.util
import java.util.Map.Entry as JEntry

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.google.common.collect.AbstractIterator
import com.google.common.primitives.UnsignedBytes
import com.typesafe.scalalogging.LazyLogging
import org.iq80.leveldb.*

class KW private (val bs: Array[Byte]) extends Comparable[KW] {
  override def equals(obj: Any): Boolean = obj match {
    case kw: KW => (kw eq this) || util.Arrays.equals(bs, kw.bs)
    case _      => false
  }

  private lazy val _hashCode = util.Arrays.hashCode(bs)

  override def hashCode(): Int = _hashCode

  override def compareTo(o: KW): Int = UnsignedBytes.lexicographicalComparator().compare(bs, o.bs)
}

object KW {
  def apply(bs: Array[Byte]): KW = new KW(bs)
}

class TestBatch extends WriteBatch {
  val newEntries     = new util.HashMap[KW, Array[Byte]]
  val deletedEntries = new util.HashSet[KW]

  override def put(key: Array[Byte], value: Array[Byte]): WriteBatch = {
    val bs = KW(key)
    deletedEntries.remove(bs)
    newEntries.put(bs, value)
    this
  }

  override def delete(key: Array[Byte]): WriteBatch = {
    val bs = KW(key)
    newEntries.remove(bs)
    deletedEntries.add(bs)
    this
  }

  override def close(): Unit = {}
}

class EW(entries: util.NavigableMap[KW, Array[Byte]]) extends AbstractIterator[JEntry[Array[Byte], Array[Byte]]] {
  private val iterator = entries.navigableKeySet().iterator()
  override def computeNext(): JEntry[Array[Byte], Array[Byte]] =
    if (iterator.hasNext) new JEntry[Array[Byte], Array[Byte]] {
      private val k                      = iterator.next()
      override def getKey: Array[Byte]   = k.bs
      override def getValue: Array[Byte] = entries.get(k)

      override def setValue(value: Array[Byte]): Array[Byte] = ???
    }
    else endOfData()
}

class TestIterator(entries: util.TreeMap[KW, Array[Byte]]) extends DBIterator {
  private var iter = new EW(entries)

  override def seek(key: Array[Byte]): Unit = {
    iter = new EW(entries.tailMap(KW(key), true))
  }

  override def seekToFirst(): Unit = {
    iter = new EW(entries)
  }

  override def peekNext(): JEntry[Array[Byte], Array[Byte]] = iter.peek()

  override def hasPrev: Boolean = ???

  override def prev(): JEntry[Array[Byte], Array[Byte]] = ???

  override def peekPrev(): JEntry[Array[Byte], Array[Byte]] = ???

  override def seekToLast(): Unit = {}

  override def close(): Unit = {}

  override def hasNext: Boolean = iter.hasNext

  override def next(): JEntry[Array[Byte], Array[Byte]] = iter.next()
}

class InMemoryDB(underlying: DB) extends DB with LazyLogging {
  private var estimatedSize: Long = 0L
  private var batchCount          = 0L
  private val entries             = new util.HashMap[KW, Array[Byte]]
  private val toDelete            = new util.HashSet[KW]

  private val cc = CacheBuilder
    .newBuilder()
    .maximumWeight(256 * 1024 * 1024)
    .weigher((_: KW, value: Array[Byte]) => value.length)
    .build[KW, Array[Byte]](new CacheLoader[KW, Array[Byte]] {
      override def load(key: KW): Array[Byte] = {
        val u = underlying.get(key.bs)
        if (u == null) Array.emptyByteArray else u
      }
    })

  private val MaxBatchSize = 512 * 1024 * 1024
  logger.info(s"Max batch size = $MaxBatchSize")

  override def get(key: Array[Byte]): Array[Byte] = {
    val kw = KW(key)
    entries.getOrDefault(
      kw,
      if (toDelete.contains(kw)) null
      else {
        val bs = cc.get(kw)
        if (bs.length == 0) null else bs
      }
    )
  }

  override def get(key: Array[Byte], options: ReadOptions): Array[Byte] = get(key)

  override def iterator(): DBIterator = iterator(new ReadOptions)

  override def iterator(options: ReadOptions): DBIterator = underlying.iterator()

  private def putAndCountBytes(key: KW, value: Array[Byte]): Unit = {
    toDelete.remove(key)
    estimatedSize += (key.bs.length + value.length)
    Option(entries.put(key, value)).foreach(bs => estimatedSize -= (key.bs.length + bs.length))
  }

  private def deleteAndCountBytes(key: KW): Unit = {
    toDelete.add(key)
    Option(entries.remove(key)).foreach(v => estimatedSize -= (key.bs.length + v.length))
  }

  override def put(key: Array[Byte], value: Array[Byte]): Unit = putAndCountBytes(KW(key), value)

  override def delete(key: Array[Byte]): Unit = deleteAndCountBytes(KW(key))

  private def flush(): Unit = {
    logger.info(s"${toDelete.size} keys to delete, ${entries.size} to add")
    underlying.suspendCompactions()
    underlying.readWrite { rw =>
      toDelete.forEach(k => rw.delete(k.bs))
      entries.forEach { (k, v) =>
        rw.put(k.bs, v)
      }
    }
    cc.invalidateAll()
    underlying.resumeCompactions()
    logger.info("Finished persisting")
    entries.clear()
    toDelete.clear()
    estimatedSize = 0L
  }

  override def write(updates: WriteBatch): Unit = updates match {
    case tb: TestBatch =>
      batchCount += 1
      tb.deletedEntries.forEach(deleteAndCountBytes)
      tb.newEntries.forEach(putAndCountBytes)

      if (estimatedSize > MaxBatchSize) {
        flush()
      }

    case _ => ???
  }

  override def createWriteBatch(): WriteBatch = new TestBatch

  override def put(key: Array[Byte], value: Array[Byte], options: WriteOptions): Snapshot = {
    put(key, value)
    getSnapshot
  }

  override def delete(key: Array[Byte], options: WriteOptions): Snapshot = {
    delete(key)
    getSnapshot
  }

  override def write(updates: WriteBatch, options: WriteOptions): Snapshot = {
    write(updates)
    getSnapshot
  }

  override def getSnapshot: Snapshot = () => ()

  override def getApproximateSizes(ranges: Range*): Array[Long] = ???

  override def getProperty(name: String): String = ""

  override def suspendCompactions(): Unit = {}

  override def resumeCompactions(): Unit = {}

  override def compactRange(begin: Array[Byte], end: Array[Byte]): Unit = {}

  override def close(): Unit = flush()
}
