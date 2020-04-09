package com.wavesplatform.database

import java.util.Map.{Entry => JEntry}

import com.google.common.collect.AbstractIterator
import com.wavesplatform.common.state.ByteStr
import org.iq80.leveldb
import org.iq80.leveldb._

import scala.collection.mutable

class TestBatch extends WriteBatch {
  val newEntries: mutable.SortedMap[ByteStr, Array[Byte]] = mutable.SortedMap.empty
  val deletedEntries: mutable.SortedSet[ByteStr] = mutable.SortedSet.empty

  override def put(key: Array[Byte], value: Array[Byte]): WriteBatch = {
    val bs = ByteStr(key)
    deletedEntries -= bs
    newEntries += bs -> value
    this
  }

  override def delete(key: Array[Byte]): WriteBatch = {
    val bs = ByteStr(key)
    newEntries -= bs
    deletedEntries += bs
    this
  }

  override def close(): Unit = {}
}

class EW(entries: mutable.SortedMap[ByteStr, Array[Byte]]) extends AbstractIterator[JEntry[Array[Byte], Array[Byte]]] {
  private val iterator = entries.iterator
  override def computeNext(): JEntry[Array[Byte], Array[Byte]] =
    if (iterator.hasNext) new JEntry[Array[Byte], Array[Byte]] {
      val (k, v) = iterator.next()

      override def getKey: Array[Byte]   = k.arr
      override def getValue: Array[Byte] = v

      override def setValue(value: Array[Byte]): Array[Byte] = ???
    } else endOfData()
}

class TestIterator(entries: mutable.SortedMap[ByteStr, Array[Byte]]) extends DBIterator {
  private var iter = new EW(entries)

  override def seek(key: Array[Byte]): Unit = {
    iter = new EW(entries.from(ByteStr(key)))
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

class TestDB extends DB {
  val entries: mutable.SortedMap[ByteStr, Array[Byte]] = mutable.SortedMap.empty

  override def get(key: Array[Byte]): Array[Byte] = entries.get(ByteStr(key)).orNull

  override def get(key: Array[Byte], options: ReadOptions): Array[Byte] = get(key)

  override def iterator(): DBIterator = new TestIterator(entries)

  override def iterator(options: ReadOptions): DBIterator = new TestIterator(entries)

  override def put(key: Array[Byte], value: Array[Byte]): Unit = entries += ByteStr(key) -> value

  override def delete(key: Array[Byte]): Unit = entries -= ByteStr(key)

  override def write(updates: WriteBatch): Unit = updates match {
    case tb: TestBatch =>
      entries ++= tb.newEntries
      entries --= tb.deletedEntries
    case _             =>
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
