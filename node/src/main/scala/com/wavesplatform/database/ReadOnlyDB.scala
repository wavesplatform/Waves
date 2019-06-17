package com.wavesplatform.database

import com.google.common.primitives.{Bytes, Ints, Shorts}
import com.wavesplatform.database.Key.KeyPrefix
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

  def iterateOverStream(prefix: Array[Byte], suffix: Array[Byte] = Array.emptyByteArray): CloseableIterator[DBEntry] = db.iterateOverStream(prefix, suffix)

  def iterateOverStream(key: KeyPrefix[_]): CloseableIterator[DBEntry] = db.iterateOverStream(key.stableBytes)

  def iterateOverStream(prefix: Short): CloseableIterator[DBEntry] = db.iterateOverStream(Shorts.toByteArray(prefix))

  def lastEntry(prefix: Short, bytes: Array[Byte], height: Int): Option[DBEntry] = {
    val stableBytes = Bytes.concat(
      Shorts.toByteArray(prefix),
      bytes
    )

    db.iterateOverStream(stableBytes)
      .takeWhile(e => Ints.fromByteArray(e.getKey.takeRight(Ints.BYTES)) <= height)
      .closeAfter(_.foldLeft(Option.empty[DBEntry])((_, last) => Option(last)))
  }

  def lastEntry(prefix: KeyPrefix[_], height: Int): Option[DBEntry] =
    lastEntry(prefix.shortPrefix, prefix.bytesPrefix, height)

  def lastKeyValue[V](prefix: KeyPrefix[V], height: Int): Option[(Array[Byte], V)] =
    lastEntry(prefix, height).map(e => (e.getKey.drop(prefix.stableBytes.length), prefix.parseF(e.getValue)))

  def lastValue[V](prefix: KeyPrefix[V], height: Int): Option[V] =
    lastKeyValue(prefix, height).map(_._2)

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
