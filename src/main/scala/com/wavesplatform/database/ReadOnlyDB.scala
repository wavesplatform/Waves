package com.wavesplatform.database

import com.wavesplatform.database.LevelDBWriter.Key
import org.iq80.leveldb.{DB, ReadOptions}

class ReadOnlyDB(db: DB, readOptions: ReadOptions) {
  def get[V](key: Key[V]): V = key.parse(db.get(key.keyBytes, readOptions))
}