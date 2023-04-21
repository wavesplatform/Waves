package com.wavesplatform.db

import org.rocksdb.{ReadOptions, RocksDB, RocksIterator, Snapshot}

import java.util.concurrent.locks.ReentrantLock

case class InterferableDB(db: RocksDB, startRead: ReentrantLock) extends RocksDB(db.getNativeHandle) {
  override def getSnapshot: Snapshot = db.getSnapshot
  override def close(): Unit         = db.close()

  override def get(key: Array[Byte]): Array[Byte]                       = ???
  override def delete(key: Array[Byte]): Unit                           = ???
  override def getProperty(name: String): String                        = ???
  override def compactRange(begin: Array[Byte], end: Array[Byte]): Unit = ???
  override def newIterator(): RocksIterator                             = ???

  override def newIterator(options: ReadOptions): RocksIterator = new RocksIterator(db, db.newIterator(options).getNativeHandle) {
    startRead.lock()

    override def next(): Unit                 = super.next()
    override def close(): Unit                = super.close()
    override def seek(key: Array[Byte]): Unit = super.seek(key)
    override def isValid: Boolean             = super.isValid

    override def seekToFirst(): Unit = super.seekToFirst()
    override def prev(): Unit        = super.prev()
    override def seekToLast(): Unit  = super.seekToLast()
  }
}
