package com.wavesplatform.ride.runner.caches.disk

import com.wavesplatform.ride.runner.caches.RemoteData
import com.wavesplatform.ride.runner.db.{ReadOnly, ReadWrite}
import com.wavesplatform.state.Height

import scala.collection.concurrent.TrieMap

class InMemWithoutHeightDiskCache[KeyT, ValueT] extends DiskCache[KeyT, ValueT] {
  private val entries = new TrieMap[KeyT, (Height, RemoteData[ValueT])]()

  def get(key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT] = get(Height(Int.MaxValue), key)

  override def get(maxHeight: Height, key: KeyT)(implicit ctx: ReadOnly): RemoteData[ValueT] =
    entries.get(key).fold[RemoteData[ValueT]](RemoteData.Unknown)(_._2)

  override def set(atHeight: Height, key: KeyT, data: RemoteData[ValueT])(implicit ctx: ReadWrite): Unit = entries.update(key, (atHeight, data))

  override def removeFrom(fromHeight: Height, key: KeyT)(implicit ctx: ReadWrite): RemoteData[ValueT] = entries.get(key) match {
    case Some((h, x)) if h >= fromHeight => entries.remove(key); x
    case Some((_, x))                    => x
    case _                               => RemoteData.Unknown
  }

  override def removeAllFrom(fromHeight: Height)(implicit ctx: ReadWrite): List[KeyT] =
    entries.collect { case (k, (h, v)) if h >= fromHeight => entries.remove(k); k }.toList
}
