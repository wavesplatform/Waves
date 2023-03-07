package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.blockchain.RemoteData
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite

trait PersistentCache[KeyT, ValueT] {
  def get(maxHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]
  def set(atHeight: Int, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit
  def remove(fromHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]
}

object PersistentCache {
  def empty[KeyT, ValueT]: PersistentCache[KeyT, ValueT] = new PersistentCache[KeyT, ValueT] {
    def get(maxHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]            = RemoteData.Unknown
    def set(atHeight: Int, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit = {}
    def remove(fromHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]        = RemoteData.Unknown
  }
}
