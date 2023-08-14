package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

trait PersistentCache[KeyT, ValueT] {
  def get(maxHeight: Height, key: KeyT)(implicit ctx: ReadOnly): RemoteData[ValueT]

  def set(atHeight: Height, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit

  /** @return The latest present (after the deletion) record */
  def removeFrom(fromHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]

  /** @return Removed keys */
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[KeyT]
}

object PersistentCache {
  def empty[KeyT, ValueT]: PersistentCache[KeyT, ValueT] = new PersistentCache[KeyT, ValueT] {
    override def get(maxHeight: Height, key: KeyT)(implicit ctx: ReadOnly): RemoteData[ValueT]             = RemoteData.Unknown
    override def set(atHeight: Height, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit = {}
    override def removeFrom(fromHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]    = RemoteData.Unknown
    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[KeyT]                    = List.empty
  }
}
