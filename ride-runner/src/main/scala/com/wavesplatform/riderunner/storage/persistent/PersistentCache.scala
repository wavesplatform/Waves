package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.blockchain.RemoteData

trait PersistentCache[KeyT, ValueT] {
  def get(maxHeight: Int, key: KeyT): RemoteData[ValueT]
  def set(atHeight: Int, key: KeyT, data: RemoteData[ValueT]): Unit
  def remove(fromHeight: Int, key: KeyT): RemoteData[ValueT]
}

object PersistentCache {
  def empty[KeyT, ValueT]: PersistentCache[KeyT, ValueT] = new PersistentCache[KeyT, ValueT] {
    override def get(maxHeight: Int, key: KeyT): RemoteData[ValueT]            = RemoteData.Unknown
    override def set(atHeight: Int, key: KeyT, data: RemoteData[ValueT]): Unit = {}
    override def remove(fromHeight: Int, key: KeyT): RemoteData[ValueT]        = RemoteData.Unknown
  }
}
