package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.riderunner.storage.RemoteData
import com.wavesplatform.riderunner.storage.StorageContext.{ReadOnly, ReadWrite}

trait PersistentCache[KeyT, ValueT] {
  def getAllKeys()(implicit ctx: ReadOnly): List[KeyT]
  def get(maxHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]
  def set(atHeight: Int, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit
  def remove(fromHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]
}

object PersistentCache {
  def empty[KeyT, ValueT]: PersistentCache[KeyT, ValueT] = new PersistentCache[KeyT, ValueT] {
    override def getAllKeys()(implicit ctx: ReadOnly): List[KeyT]                                       = List.empty
    override def get(maxHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]            = RemoteData.Unknown
    override def set(atHeight: Int, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit = {}
    override def remove(fromHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]        = RemoteData.Unknown
  }
}
