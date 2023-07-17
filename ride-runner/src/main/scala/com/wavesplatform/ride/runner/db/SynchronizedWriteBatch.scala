package com.wavesplatform.ride.runner.db

import org.rocksdb.WriteBatch

/** WriteBatch must be synchronized, see its documentation.
  */
class SynchronizedWriteBatch(private[db] val underlying: WriteBatch) extends AutoCloseable {
  def use(f: WriteBatch => Unit): Unit = synchronized(f(underlying))
  override def close(): Unit           = synchronized(underlying.close())
}

object SynchronizedWriteBatch {
  def apply(): SynchronizedWriteBatch = new SynchronizedWriteBatch(new WriteBatch())
}
