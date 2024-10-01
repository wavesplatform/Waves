package com.wavesplatform.ride.runner.db

import com.wavesplatform.database.Key
import org.rocksdb.{ColumnFamilyHandle, RocksDB}

trait HasDb {
  protected def db: RocksDB

  protected def getCFH(key: Key[?]): ColumnFamilyHandle                           = orDefaultCFH(key.columnFamilyHandle)
  protected def orDefaultCFH(cfh: Option[ColumnFamilyHandle]): ColumnFamilyHandle = cfh.getOrElse(db.getDefaultColumnFamily)
}
