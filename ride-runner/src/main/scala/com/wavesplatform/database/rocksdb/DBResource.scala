package com.wavesplatform.database.rocksdb

import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

import scala.collection.View
import scala.collection.mutable.ArrayBuffer

trait DBResource extends AutoCloseable {
  def get[V](key: Key[V]): V
  def get(key: Array[Byte]): Array[Byte]
  def multiGet[A](keys: ArrayBuffer[Key[A]], valBufferSizes: ArrayBuffer[Int]): View[A]
  def multiGet[A](keys: ArrayBuffer[Key[A]], valBufferSize: Int): View[A]
  def multiGetFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSizes: ArrayBuffer[Int]): Seq[A]
  def prefixIterator: RocksIterator // Should have a single instance
  def fullIterator: RocksIterator
  def withSafePrefixIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A = ()): A
  def withSafeFullIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A = ()): A
}

object DBResource {
  def apply(db: RocksDB): DBResource = new DBResource {
    private[this] val snapshot    = db.getSnapshot
    private[this] val readOptions = new ReadOptions().setSnapshot(snapshot).setVerifyChecksums(false)

    override def get[V](key: Key[V]): V = key.parse(db.get(readOptions, key.keyBytes))

    override def get(key: Array[Byte]): Array[Byte] = db.get(readOptions, key)

    override def multiGetFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSizes: ArrayBuffer[Int]): Seq[A] =
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
    override lazy val prefixIterator: RocksIterator = {
      prefixIteratorWasOpened = true
      db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))
    }

    @volatile private var fullIteratorWasOpened = false
    override lazy val fullIterator: RocksIterator = {
      fullIteratorWasOpened = true
      db.newIterator(readOptions.setTotalOrderSeek(true))
    }

    override def withSafePrefixIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A): A = prefixIterator.synchronized {
      if (prefixIterator.isOwningHandle) ifNotClosed(prefixIterator) else ifClosed
    }

    override def withSafeFullIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A): A = fullIterator.synchronized {
      if (fullIterator.isOwningHandle) ifNotClosed(fullIterator) else ifClosed
    }

    override def close(): Unit = {
      if (prefixIteratorWasOpened) prefixIterator.synchronized(prefixIterator.close())
      if (fullIteratorWasOpened) fullIterator.synchronized(fullIterator.close())
      snapshot.close()
      readOptions.close()
    }
  }
}
