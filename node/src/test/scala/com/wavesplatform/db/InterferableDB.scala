package com.wavesplatform.db

import org.rocksdb.{ColumnFamilyHandle, ReadOptions, RocksDB, RocksIterator}

import java.util.concurrent.locks.Lock

case class InterferableDB(db: RocksDB, startRead: Lock) extends RocksDB(db.getNativeHandle) {
  override def getDefaultColumnFamily: ColumnFamilyHandle = db.getDefaultColumnFamily

  override def newIterator(columnFamilyHandle: ColumnFamilyHandle, readOptions: ReadOptions): RocksIterator = {
    startRead.lock()
    db.newIterator(columnFamilyHandle, readOptions)
  }

  override def newIterator(options: ReadOptions): RocksIterator = {
    startRead.lock()
    db.newIterator(options)
  }
}
