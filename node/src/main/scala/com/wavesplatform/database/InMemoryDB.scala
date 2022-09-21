package com.wavesplatform.database

import java.util
import java.util.Map.Entry as JEntry

import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.google.common.collect.AbstractIterator
import com.google.common.primitives.UnsignedBytes
import com.typesafe.scalalogging.LazyLogging
import com.wavesplatform.database.InMemoryDB.ByteArrayHashingStrategy
import org.eclipse.collections.api.block.HashingStrategy
import org.eclipse.collections.impl.factory.{HashingStrategyMaps, HashingStrategySets}
import org.iq80.leveldb.*

import scala.compat.java8.FunctionConverters.*

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
  val newEntries     = HashingStrategyMaps.mutable.`with`[Array[Byte], Array[Byte]](ByteArrayHashingStrategy)
  val deletedEntries = HashingStrategySets.mutable.`with`[Array[Byte]](ByteArrayHashingStrategy)

  override def put(key: Array[Byte], value: Array[Byte]): WriteBatch = {
    deletedEntries.remove(key)
    newEntries.put(key, value)
    this
  }

  override def delete(key: Array[Byte]): WriteBatch = {
    newEntries.remove(key)
    deletedEntries.add(key)
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

object InMemoryDB {
  object ByteArrayHashingStrategy extends HashingStrategy[Array[Byte]] {
    override def computeHashCode(obj: Array[Byte]): Int = util.Arrays.hashCode(obj)

    override def equals(object1: Array[Byte], object2: Array[Byte]): Boolean = util.Arrays.equals(object1, object2)
  }
}

class InMemoryDB(underlying: DB) extends DB with LazyLogging {
  import InMemoryDB.*

  private var estimatedSize: Long = 0L
  private var batchCount          = 0L
  private val entries             = HashingStrategyMaps.mutable.`with`[Array[Byte], Array[Byte]](ByteArrayHashingStrategy)
  private val toDelete            = HashingStrategySets.mutable.`with`[Array[Byte]](ByteArrayHashingStrategy)

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

  override def get(key: Array[Byte]): Array[Byte] =
    entries.getOrDefault(
      key,
      if (toDelete.contains(key)) null
      else {
        val kw = KW(key)
        val bs = cc.get(kw)
        if (bs.length == 0) null else bs
      }
    )

  override def get(key: Array[Byte], options: ReadOptions): Array[Byte] = get(key)

  override def iterator(): DBIterator = iterator(new ReadOptions)

  override def iterator(options: ReadOptions): DBIterator = underlying.iterator()

  private def putAndCountBytes(key: Array[Byte], value: Array[Byte]): Unit = {
    toDelete.remove(key)
    val entrySize = key.length + value.length
    estimatedSize += entrySize
    Option(entries.put(key, value)).foreach(bs => estimatedSize -= (key.length + bs.length))
  }

  private def deleteAndCountBytes(key: Array[Byte]): Unit = {
    toDelete.add(key)
    Option(entries.remove(key)).foreach(v => estimatedSize -= (key.length + v.length))
  }

  override def put(key: Array[Byte], value: Array[Byte]): Unit = putAndCountBytes(key, value)

  override def delete(key: Array[Byte]): Unit = deleteAndCountBytes(key)

  private def flush(): Unit = {
    logger.info(s"${toDelete.size} keys to delete, ${entries.size} to add")
    underlying.suspendCompactions()
    underlying.readWrite { rw =>
      toDelete.forEach(((t: Array[Byte]) => rw.delete(t)).asJava)
      entries.forEach({ (k: Array[Byte], v: Array[Byte]) =>
        rw.put(k, v)
      }.asJava)
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
      tb.deletedEntries.forEach((k => deleteAndCountBytes(k)).asJava)
      tb.newEntries.forEach((k, v) => putAndCountBytes(k, v))

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
