package com.wavesplatform.ride.blockchain

import scala.collection.mutable

// TODO trait with different collections: LongMap, AnyRefMap, etc...

class ReadOnlyRideData[KeyT <: AnyRef, ValueT, TagT](loader: KeyT => Option[ValueT]) {
  protected val map = mutable.AnyRefMap.empty[KeyT, TaggedData[BlockchainData[ValueT], TagT]]

  def get(key: KeyT, tag: TagT): Option[ValueT] = map
    .updateWith(key) {
      case Some(orig) => Some(orig.withTag(tag))
      case None       => Some(TaggedData(BlockchainData.loaded[ValueT](loader(key)), Set(tag)))
    }
    .flatMap(_.data.mayBeValue)
}

class RideData[KeyT <: AnyRef, ValueT, TagT](loader: KeyT => Option[ValueT]) extends ReadOnlyRideData[KeyT, ValueT, TagT](loader) {
  def replace(key: KeyT)(remap: Option[ValueT] => Option[ValueT]): Set[TagT] =
    map.get(key) match {
      case None => Set.empty
      case Some(orig) =>
        val updated = BlockchainData.loaded(remap(orig.data.mayBeValue))
        if (updated == orig.data) Set.empty
        else {
          map.update(key, orig.copy(data = updated))
          orig.tags
        }
    }
}
