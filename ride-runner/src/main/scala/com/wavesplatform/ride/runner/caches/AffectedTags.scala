package com.wavesplatform.ride.runner.caches

case class AffectedTags[TagT](xs: Set[TagT]) {
  def ++(that: AffectedTags[TagT]): AffectedTags[TagT] =
    if (that.isEmpty) this
    else if (isEmpty) that
    else if (xs.size <= that.xs.size) AffectedTags(that.xs ++ xs)
    else AffectedTags(xs ++ that.xs)

  def isEmpty = xs.isEmpty
}

object AffectedTags {
  def empty[TagT]: AffectedTags[TagT] = new AffectedTags[TagT](Set.empty)
}
