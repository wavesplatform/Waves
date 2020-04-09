package com.wavesplatform.database

import java.util.Map.{Entry => JEntry}

import com.google.common.collect.AbstractIterator
import com.google.common.primitives.UnsignedBytes
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb
import org.iq80.leveldb._

import scala.collection.mutable

class KW(val bs: Array[Byte]) extends Comparable[KW] {
  override def equals(obj: Any): Boolean = super.equals(obj)

  override def compareTo(o: KW): Int = UnsignedBytes.lexicographicalComparator().compare(bs, o.bs)
}

object KW {
  def apply(bs: Array[Byte]): KW = new KW(bs)
}

class TestBatch extends WriteBatch {
  val newEntries: mutable.SortedMap[KW, Array[Byte]] = mutable.SortedMap.empty
  val deletedEntries: mutable.SortedSet[KW]          = mutable.SortedSet.empty

  override def put(key: Array[Byte], value: Array[Byte]): WriteBatch = {
    val bs = KW(key)
    deletedEntries -= bs
    newEntries += bs -> value
    this
  }

  override def delete(key: Array[Byte]): WriteBatch = {
    val bs = KW(key)
    newEntries -= bs
    deletedEntries += bs
    this
  }

  override def close(): Unit = {}
}

class EW(entries: mutable.SortedMap[KW, Array[Byte]]) extends AbstractIterator[JEntry[Array[Byte], Array[Byte]]] {
  private val iterator = entries.iterator
  override def computeNext(): JEntry[Array[Byte], Array[Byte]] =
    if (iterator.hasNext) new JEntry[Array[Byte], Array[Byte]] {
      val (k, v) = iterator.next()

      override def getKey: Array[Byte]   = k.bs
      override def getValue: Array[Byte] = v

      override def setValue(value: Array[Byte]): Array[Byte] = ???
    } else endOfData()
}

class TestIterator(entries: mutable.SortedMap[KW, Array[Byte]]) extends DBIterator {
  private var iter = new EW(entries)

  override def seek(key: Array[Byte]): Unit = {
    iter = new EW(entries.from(KW(key)))
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

class InMemoryDB(underlying: DB) extends DB with ScorexLogging {
  private var estimatedSize: Long                         = 0L
  private var batchCount                                  = 0L
  private val entries: mutable.SortedMap[KW, Array[Byte]] = mutable.SortedMap.empty
  private val toDelete: mutable.SortedSet[KW]             = mutable.SortedSet.empty

  override def get(key: Array[Byte]): Array[Byte] = entries.getOrElse(KW(key), underlying.get(key))

  override def get(key: Array[Byte], options: ReadOptions): Array[Byte] = get(key)

  override def iterator(): DBIterator = new TestIterator(entries)

  override def iterator(options: ReadOptions): DBIterator = new TestIterator(entries)

  private def putAndCountBytes(key: KW, value: Array[Byte]): Unit = {
    toDelete -= key
    estimatedSize += (key.bs.length + value.length)
    entries += key -> value
  }

  private def deleteAndCountBytes(key: KW): Unit = {
    toDelete += key
    entries.remove(key).foreach(v => estimatedSize -= (key.bs.length + v.length))
  }

  override def put(key: Array[Byte], value: Array[Byte]): Unit = putAndCountBytes(KW(key), value)

  override def delete(key: Array[Byte]): Unit = deleteAndCountBytes(KW(key))

  override def write(updates: WriteBatch): Unit = updates match {
    case tb: TestBatch =>
      batchCount += 1
      tb.deletedEntries.foreach(deleteAndCountBytes)
      for ((k, v) <- tb.newEntries) {
        putAndCountBytes(k, v)
      }

      if (estimatedSize > 1024 * 1024 * 1024) {
        log.info(s"${toDelete.size} keys to delete, ${entries.size} to add")
        underlying.readWrite { rw =>
          toDelete.foreach(k => rw.delete(k.bs))
          for ((k, v) <- entries) {
            rw.put(k.bs, v)
          }
        }
        log.info("Finished persisting")
        entries.clear()
        toDelete.clear()
        estimatedSize = 0L
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

  override def getApproximateSizes(ranges: leveldb.Range*): Array[Long] = Array.emptyLongArray

  override def getProperty(name: String): String = ""

  override def suspendCompactions(): Unit = {}

  override def resumeCompactions(): Unit = {}

  override def compactRange(begin: Array[Byte], end: Array[Byte]): Unit = {}

  override def close(): Unit = {}
}
