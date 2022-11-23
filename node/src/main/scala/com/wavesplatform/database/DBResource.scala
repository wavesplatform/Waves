package com.wavesplatform.database

import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

import scala.collection.View
import scala.collection.mutable.ArrayBuffer

trait DBResource extends AutoCloseable {
  def get[V](key: Key[V]): V
  def get(key: Array[Byte]): Array[Byte]
  def multiGet[V, K](keys: ArrayBuffer[(Key[V], K)]): View[(V, K)]
  def multiGet(keys: ArrayBuffer[Array[Byte]]): Seq[Array[Byte]]
  def multiGet(keys: Seq[Array[Byte]]): Seq[Array[Byte]]
  def multiGetInts(keys: ArrayBuffer[Array[Byte]]): View[Option[Int]]
  def multiGetLongs(keys: Seq[Array[Byte]]): View[Long]
  def multiGetLongs(keys: ArrayBuffer[Array[Byte]]): View[Long]
  def multiGetBuffered[A](keys: ArrayBuffer[Key[A]], valBufferSizes: ArrayBuffer[Int]): Seq[A]
  def multiGetBufferedFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSize: Int): Seq[A]
  def multiGetBufferedFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSizes: ArrayBuffer[Int]): Seq[A]
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

    override def multiGet[V, K](keys: ArrayBuffer[(Key[V], K)]): View[(V, K)] = db.multiGet(readOptions, keys)

    override def multiGetBufferedFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSize: Int): Seq[A] =
      db.multiGetBufferedFlat(readOptions, keys, valBufferSize)

    override def multiGetBufferedFlat[A](keys: ArrayBuffer[Key[Option[A]]], valBufferSizes: ArrayBuffer[Int]): Seq[A] =
      db.multiGetBufferedFlat(readOptions, keys, valBufferSizes)

    def multiGetBuffered[A](keys: ArrayBuffer[Key[A]], valBufferSizes: ArrayBuffer[Int]): Seq[A] =
      db.multiGetBuffered(readOptions, keys, valBufferSizes)

    override def multiGet(keys: ArrayBuffer[Array[Byte]]): Seq[Array[Byte]] = db.multiGet(readOptions, keys)

    override def multiGet(keys: Seq[Array[Byte]]): Seq[Array[Byte]] = db.multiGet(readOptions, keys)

    override def multiGetInts(keys: ArrayBuffer[Array[Byte]]): View[Option[Int]] = db.multiGetInts(readOptions, keys)

    override def multiGetLongs(keys: Seq[Array[Byte]]): View[Long] = db.multiGetLongs(readOptions, keys)

    override def multiGetLongs(keys: ArrayBuffer[Array[Byte]]): View[Long] = db.multiGetLongs(readOptions, keys)

    override lazy val prefixIterator: RocksIterator = db.newIterator(readOptions.setTotalOrderSeek(false).setPrefixSameAsStart(true))

    override lazy val fullIterator: RocksIterator = db.newIterator(readOptions.setTotalOrderSeek(true))

    override def withSafePrefixIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A): A = prefixIterator.synchronized {
      if (prefixIterator.isOwningHandle) ifNotClosed(prefixIterator) else ifClosed
    }

    override def withSafeFullIterator[A](ifNotClosed: RocksIterator => A)(ifClosed: => A): A = fullIterator.synchronized {
      if (fullIterator.isOwningHandle) ifNotClosed(fullIterator) else ifClosed
    }

    override def close(): Unit = {
      prefixIterator.synchronized(prefixIterator.close())
      fullIterator.synchronized(fullIterator.close())
      snapshot.close()
      readOptions.close()
    }
  }
}
