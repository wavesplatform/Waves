package com.wavesplatform.database

import com.wavesplatform.metrics.LevelDBStats
import com.wavesplatform.metrics.LevelDBStats.DbHistogramExt
import org.iq80.leveldb.{DB, DBIterator, ReadOptions}

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
}
