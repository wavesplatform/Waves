package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.state.Height

trait PersistentCache[KeyT, ValueT] {
  def getAllKeys()(implicit ctx: ReadOnly): List[KeyT]
  def get(maxHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]
  def set(atHeight: Height, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit
  def removeFrom(fromHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]
  def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[KeyT]
}

object PersistentCache {
  def empty[KeyT, ValueT]: PersistentCache[KeyT, ValueT] = new PersistentCache[KeyT, ValueT] {
    override def getAllKeys()(implicit ctx: ReadOnly): List[KeyT]                                          = List.empty
    override def get(maxHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]            = RemoteData.Unknown
    override def set(atHeight: Height, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit = {}
    override def removeFrom(fromHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]    = RemoteData.Unknown
    override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[KeyT]                    = List.empty
  }
}
