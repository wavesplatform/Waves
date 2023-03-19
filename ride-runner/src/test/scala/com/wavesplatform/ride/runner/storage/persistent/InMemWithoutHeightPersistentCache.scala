package com.wavesplatform.ride.runner.storage.persistent

import com.wavesplatform.ride.runner.storage.RemoteData
import com.wavesplatform.ride.runner.storage.persistent.PersistentStorageContext.{ReadOnly, ReadWrite}

import scala.collection.concurrent.TrieMap

class InMemWithoutHeightPersistentCache[KeyT, ValueT] extends PersistentCache[KeyT, ValueT] {
  private val entries = new TrieMap[KeyT, RemoteData[ValueT]]()

  def get(key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT] = get(Int.MaxValue, key)

  override def getAllKeys()(implicit ctx: ReadOnly): List[KeyT]                                       = List.empty
  override def get(maxHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT]            = entries.getOrElse(key, RemoteData.Unknown)
  override def set(atHeight: Int, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit = entries.update(key, data)
  override def remove(fromHeight: Int, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT] = entries.remove(key).getOrElse(RemoteData.Unknown)
}
