package com.wavesplatform.db
import org.iq80.leveldb
import org.iq80.leveldb.*

import java.util.Map
import java.util.concurrent.locks.ReentrantLock

case class InterferableDB(db: DB, startRead: ReentrantLock) extends DB {
  override def get(key: Array[Byte]): Array[Byte]                       = { startRead.lock(); db.get(key) }
  override def get(key: Array[Byte], options: ReadOptions): Array[Byte] = db.get(key, options)
  override def put(key: Array[Byte], value: Array[Byte]): Unit          = db.put(key, value)
  override def getSnapshot: Snapshot                                    = db.getSnapshot
  override def close(): Unit                                            = db.close()

  override def delete(key: Array[Byte]): Unit                                             = ???
  override def write(updates: WriteBatch): Unit                                           = ???
  override def createWriteBatch(): WriteBatch                                             = ???
  override def put(key: Array[Byte], value: Array[Byte], options: WriteOptions): Snapshot = ???
  override def delete(key: Array[Byte], options: WriteOptions): Snapshot                  = ???
  override def write(updates: WriteBatch, options: WriteOptions): Snapshot                = ???
  override def getApproximateSizes(ranges: leveldb.Range*): Array[Long]                   = ???
  override def getProperty(name: String): String                                          = ???
  override def suspendCompactions(): Unit                                                 = ???
  override def resumeCompactions(): Unit                                                  = ???
  override def compactRange(begin: Array[Byte], end: Array[Byte]): Unit                   = ???
  override def iterator(): DBIterator                                                     = ???

  override def iterator(options: ReadOptions): DBIterator = new DBIterator {
    private val iterator = db.iterator()
    startRead.lock()

    override def next(): Map.Entry[Array[Byte], Array[Byte]] = iterator.next()
    override def close(): Unit                               = iterator.close()
    override def seek(key: Array[Byte]): Unit                = iterator.seek(key)
    override def hasNext: Boolean                            = iterator.hasNext

    override def seekToFirst(): Unit                             = ???
    override def peekNext(): Map.Entry[Array[Byte], Array[Byte]] = ???
    override def hasPrev: Boolean                                = ???
    override def prev(): Map.Entry[Array[Byte], Array[Byte]]     = ???
    override def peekPrev(): Map.Entry[Array[Byte], Array[Byte]] = ???
    override def seekToLast(): Unit                              = ???
  }
}
