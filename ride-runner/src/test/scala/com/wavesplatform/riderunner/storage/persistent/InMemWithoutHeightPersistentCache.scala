package com.wavesplatform.riderunner.storage.persistent

import com.wavesplatform.blockchain.RemoteData

import scala.collection.concurrent.TrieMap

class InMemWithoutHeightPersistentCache[KeyT, ValueT] extends PersistentCache[KeyT, ValueT] {
  private val entries = new TrieMap[KeyT, RemoteData[ValueT]]()

  def get(key: KeyT): RemoteData[ValueT]                                     = get(Int.MaxValue, key)
  override def get(maxHeight: Int, key: KeyT): RemoteData[ValueT]            = entries.getOrElse(key, RemoteData.Unknown)
  override def set(atHeight: Int, key: KeyT, data: RemoteData[ValueT]): Unit = entries.update(key, data)
  override def remove(fromHeight: Int, key: KeyT): RemoteData[ValueT]        = entries.remove(key).getOrElse(RemoteData.Unknown)
}
