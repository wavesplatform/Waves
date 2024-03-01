package com.wavesplatform.database

import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB, RocksIterator}

import scala.collection.View
import scala.collection.mutable.ArrayBuffer

trait DBResource extends AutoCloseable {
  def get[V](key: Key[V]): V
  def get(key: Array[Byte]): Array[Byte]
  def multiGet[A](keys: ArrayBuffer[Key[A]], valBufferSizes: ArrayBuffer[Int]): View[A]
  def multiGet[A](keys: ArrayBuffer[Key[A]], valBufferSize: Int): View[A]
  def multiGetFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSizes: ArrayBuffer[Int]): Seq[A]
  def prefixIterator: RocksIterator // Should have a single instance
  def fullIterator: RocksIterator   // Should have a single instance
  def withSafePrefixIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A = ()): A
  def withSafeFullIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A = ()): A
}

object DBResource {
  def apply(db: RocksDB, iteratorCfHandle: Option[ColumnFamilyHandle] = None): DBResource = new DBResource {
    private[this] val snapshot    = db.getSnapshot
    private[this] val readOptions = new ReadOptions().setSnapshot(snapshot).setVerifyChecksums(false)

    override def get[V](key: Key[V]): V = key.parse(db.get(key.columnFamilyHandle.getOrElse(db.getDefaultColumnFamily), readOptions, key.keyBytes))

    override def get(key: Array[Byte]): Array[Byte] = db.get(readOptions, key)

    override def multiGetFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSizes: ArrayBuffer[Int]): Seq[A] =
      db.multiGetFlat(readOptions, keys, valBufferSizes)

    def multiGet[A](keys: ArrayBuffer[Key[A]], valBufferSizes: ArrayBuffer[Int]): View[A] =
      db.multiGet(readOptions, keys, valBufferSizes)

    def multiGet[A](keys: ArrayBuffer[Key[A]], valBufferSize: Int): View[A] =
      db.multiGet(readOptions, keys, valBufferSize)

    override lazy val prefixIterator: RocksIterator =
      db.newIterator(iteratorCfHandle.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))

    override lazy val fullIterator: RocksIterator =
      db.newIterator(iteratorCfHandle.getOrElse(db.getDefaultColumnFamily), readOptions.setTotalOrderSeek(true))

    override def withSafePrefixIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A): A = prefixIterator.synchronized {
      if (prefixIterator.isOwningHandle) ifNotClosed(prefixIterator) else ifClosed
    }

    override def withSafeFullIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A): A = fullIterator.synchronized {
      if (fullIterator.isOwningHandle) ifNotClosed(fullIterator) else ifClosed
    }

    override def close(): Unit = {
      prefixIterator.synchronized(prefixIterator.close())
      fullIterator.synchronized(fullIterator.close())
      db.releaseSnapshot(snapshot)
      readOptions.close()
    }
  }
}
