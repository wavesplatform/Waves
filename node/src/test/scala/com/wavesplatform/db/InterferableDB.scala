package com.wavesplatform.db

import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

import java.util.concurrent.locks.Lock

case class InterferableDB(db: RocksDB, startRead: Lock) extends RocksDB(db.getNativeHandle) {
  override def newIterator(options: ReadOptions): RocksIterator = {
    startRead.lock()
    db.newIterator(options)
  }
}
