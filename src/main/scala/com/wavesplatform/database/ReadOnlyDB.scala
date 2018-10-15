package com.wavesplatform.database

import com.wavesplatform.metrics.LevelDBStats
import org.iq80.leveldb.{DB, DBIterator, ReadOptions}

class ReadOnlyDB(db: DB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V = LevelDBStats.read.measureForKey(key) {
    key.parse(db.get(key.keyBytes, readOptions))
  }

  def has[V](key: Key[V]): Boolean = LevelDBStats.read.measureForKey(key) {
    db.get(key.keyBytes, readOptions) != null
  }

  def iterator: DBIterator = db.iterator(readOptions)
}
