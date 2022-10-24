package com.wavesplatform.ride.blockchain

case class TaggedData[ValueT, TagT](data: ValueT, tags: Set[TagT] = Set.empty) {
  def withTag(tag: TagT): TaggedData[ValueT, TagT] = copy(tags = tags + tag)
}
