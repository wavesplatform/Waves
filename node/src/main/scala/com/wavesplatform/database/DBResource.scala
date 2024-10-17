package com.wavesplatform.database

import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB, RocksIterator}

import scala.collection.View
import scala.collection.mutable.ArrayBuffer

class DBResource(db: RocksDB, iteratorCfHandle: Option[ColumnFamilyHandle] = None) extends AutoCloseable {
  private[this] val snapshot = db.getSnapshot
  // checksum verification is **very** expensive, so it's explicitly disabled
  private[this] val readOptions = new ReadOptions().setSnapshot(snapshot).setVerifyChecksums(false)

  def get[V](key: Key[V]): V = key.parse(db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes))

  def get(key: Array[Byte]): Array[Byte] = db.get(readOptions, key)

  def multiGetFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSizes: ArrayBuffer[Int]): Seq[A] =
    db.multiGetFlat(readOptions, keys, valBufferSizes)

  def multiGet[A](keys: ArrayBuffer[Key[A]], valBufferSizes: ArrayBuffer[Int]): View[A] =
    db.multiGet(readOptions, keys, valBufferSizes)

  def multiGet[A](keys: ArrayBuffer[Key[A]], valBufferSize: Int): View[A] =
    db.multiGet(readOptions, keys, valBufferSize)

  @volatile private var prefixIteratorWasOpened = false
  /**
   * Finds the exact key for iter.seek(key) if key.length < 10 and becomes invalid on iter.next().
   * Works as intended if prefix(key).length >= 10.
   * @see RDB.newColumnFamilyOptions
   */
  lazy val prefixIterator: RocksIterator = {
    prefixIteratorWasOpened = true
    db.newIterator(iteratorCfHandle.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))
  }

  @volatile private var fullIteratorWasOpened = false
  lazy val fullIterator: RocksIterator = {
    fullIteratorWasOpened = true
    db.newIterator(iteratorCfHandle.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(true))
  }

  def withSafePrefixIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A): A = prefixIterator.synchronized {
    if (prefixIterator.isOwningHandle) ifNotClosed(prefixIterator) else ifClosed
  }

  def withSafeFullIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A): A = fullIterator.synchronized {
    if (fullIterator.isOwningHandle) ifNotClosed(fullIterator) else ifClosed
  }

  override def close(): Unit = {
    if (prefixIteratorWasOpened) prefixIterator.synchronized(prefixIterator.close())
    if (fullIteratorWasOpened) fullIterator.synchronized(fullIterator.close())
    db.releaseSnapshot(snapshot)
    readOptions.close()
  }
}
