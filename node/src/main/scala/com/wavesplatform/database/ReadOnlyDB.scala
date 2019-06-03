package com.wavesplatform.database

import com.google.common.primitives.{Bytes, Shorts}
import com.wavesplatform.metrics.LevelDBStats
import com.wavesplatform.metrics.LevelDBStats.DbHistogramExt
import com.wavesplatform.utils.CloseableIterator
import org.iq80.leveldb.{DB, DBIterator, ReadOptions}

import scala.annotation.tailrec

class ReadOnlyDB(db: DB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V = {
    val bytes = db.get(key.keyBytes, readOptions)
    LevelDBStats.read.recordTagged(key, bytes)
    key.parse(bytes)
  }

  def has[V](key: Key[V]): Boolean = {
    val bytes = db.get(key.keyBytes, readOptions)
    LevelDBStats.read.recordTagged(key, bytes)
    bytes != null
  }

  def iterator: DBIterator = db.iterator(readOptions)

  def iterateOver(prefix: Short)(f: DBEntry => Unit): Unit = db.iterateOver(prefix)(f)

  def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit) = db.iterateOver(prefix)(f)

  def iterateOverStream(): CloseableIterator[DBEntry] = db.iterateOverStream()

  def iterateOverStream(prefix: Array[Byte]): CloseableIterator[DBEntry] = db.iterateOverStream(prefix)

  def iterateOverStream(prefix: Short): CloseableIterator[DBEntry] = db.iterateOverStream(Shorts.toByteArray(prefix))

  def iterateOverStreamReverse(seekKey: Array[Byte], prefix: Array[Byte]): CloseableIterator[DBEntry] =
    db.iterateOverStreamReverse(seekKey, prefix)

  def iterateOverStreamReverse(prefix: Short, bytes: Array[Byte]): CloseableIterator[DBEntry] = {
    val prefixBytes = Shorts.toByteArray(prefix)
    val incBytes = new Array[Byte](bytes.length)
    bytes.zipWithIndex.foreach { case (b, i) => incBytes(i) = if (java.lang.Byte.toUnsignedInt(b) == 0xFF) b else (b + 1).toByte }
    db.iterateOverStreamReverse(Bytes.concat(prefixBytes, incBytes), Bytes.concat(prefixBytes, bytes))
  }

  def iterateOverStreamReverse(prefix: Short): CloseableIterator[DBEntry] =
    db.iterateOverStreamReverse(Shorts.toByteArray((prefix + 1).toShort), Shorts.toByteArray(prefix))

  def read[T](keyName: String, prefix: Array[Byte], seek: Array[Byte], n: Int)(deserialize: DBEntry => T): Vector[T] = {
    val iter = iterator
    @tailrec def loop(aux: Vector[T], restN: Int, totalBytesRead: Long): (Vector[T], Long) = {
      if (restN > 0 && iter.hasNext) {
        val elem = iter.next()
        if (elem.getKey.startsWith(prefix)) loop(aux :+ deserialize(elem), restN - 1, totalBytesRead + elem.getValue.length)
        else (aux, totalBytesRead)
      } else (aux, totalBytesRead)
    }

    try {
      iter.seek(seek)
      val (r, totalBytesRead) = loop(Vector.empty, n, 0)
      LevelDBStats.read.recordTagged(keyName, totalBytesRead)
      r
    } finally iter.close()
  }
}
