package com.wavesplatform.blockchain

import scala.collection.mutable

// TODO refactor, see DataStorage
final class RideData[KeyT, ValueT, TagT](
    map: mutable.AbstractMap[KeyT, TaggedData[RemoteData[ValueT], TagT]],
    loader: KeyT => Option[ValueT]
) {
  def get(key: KeyT, tag: TagT): Option[ValueT] = map
    .updateWith(key) {
      case Some(orig) => Some(orig.withTag(tag))
      case None       => Some(TaggedData(RemoteData.loaded[ValueT](loader(key)), Set(tag)))
    }
    .flatMap(_.data.mayBeValue)

  /** @param remap
    *   We could know, that the value does not exist
    */
  def replaceIfKnown(key: KeyT)(remap: Option[ValueT] => Option[ValueT]): Set[TagT] =
    map.get(key) match {
      case None => Set.empty
      case Some(orig) =>
        val updated = RemoteData.loaded(remap(orig.data.mayBeValue))
        if (updated == orig.data) Set.empty
        else {
          map.update(key, orig.copy(data = updated))
          orig.tags
        }
    }
}

object RideData {
  def anyRefMap[KeyT <: AnyRef, ValueT, TagT](loader: KeyT => Option[ValueT]): RideData[KeyT, ValueT, TagT] =
    new RideData(mutable.AnyRefMap.empty[KeyT, TaggedData[RemoteData[ValueT], TagT]], loader)

  def mapReadOnly[KeyT, ValueT, TagT](loader: KeyT => Option[ValueT]): RideData[KeyT, ValueT, TagT] =
    new RideData(mutable.HashMap.empty[KeyT, TaggedData[RemoteData[ValueT], TagT]], loader)
}
