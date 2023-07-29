package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.ride.runner.storage.persistent.PersistentCache.{MaxHeight, MinHeight}
import com.wavesplatform.state.Height

trait PersistentCache[KeyT, ValueT] {
  def getAllKeys()(implicit ctx: ReadOnly): List[KeyT]

  def getLatest(key: KeyT)(implicit ctx: ReadOnly): RemoteData[ValueT] = get(MaxHeight, key)
  def get(maxHeight: Height, key: KeyT)(implicit ctx: ReadOnly): RemoteData[ValueT]

  def set(atHeight: Height, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit

  def removeAll(keyT: KeyT)(implicit ctx: ReadWrite): Unit = removeFrom(MinHeight, keyT)

  /** @return The latest present (after the deletion) record */
  def removeFrom(fromHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]

  /** @return Removed keys */
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[KeyT]
}

object PersistentCache {
  private val MinHeight = Height(0)
  private val MaxHeight = Height(Int.MaxValue)

  def empty[KeyT, ValueT]: PersistentCache[KeyT, ValueT] = new PersistentCache[KeyT, ValueT] {
    override def getAllKeys()(implicit ctx: ReadOnly): List[KeyT]                                          = List.empty
    override def get(maxHeight: Height, key: KeyT)(implicit ctx: ReadOnly): RemoteData[ValueT]             = RemoteData.Unknown
    override def set(atHeight: Height, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit = {}
    override def removeFrom(fromHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]    = RemoteData.Unknown
    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[KeyT]                    = List.empty
  }
}
