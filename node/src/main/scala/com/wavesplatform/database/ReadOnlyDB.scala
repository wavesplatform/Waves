package com.wavesplatform.database

import com.wavesplatform.metrics.LevelDBStats
import com.wavesplatform.metrics.LevelDBStats.DbHistogramExt
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

  def iterateOver(prefix: Array[Byte])(f: DBEntry => Unit): Unit = db.iterateOver(prefix)(f)

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
