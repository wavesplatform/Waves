package com.wavesplatform.storage

import com.wavesplatform.blockchain.{RemoteData, TaggedData}

import scala.collection.mutable

trait HasAnyRefMap[KeyT <: AnyRef, ValueT, TagT] { this: HeightStorage[KeyT, ValueT, TagT] =>
  override protected val memoryCache = mutable.AnyRefMap.empty[KeyT, TaggedData[RemoteData[ValueT], TagT]]
}
