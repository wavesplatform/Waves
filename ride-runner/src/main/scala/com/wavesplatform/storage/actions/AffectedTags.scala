package com.wavesplatform.storage.actions

case class AffectedTags[TagT](xs: Set[TagT]) {
  def ++(ys: AffectedTags[TagT]): AffectedTags[TagT] = AffectedTags(xs ++ ys.xs)
  def isEmpty = xs.isEmpty
}

object AffectedTags {
  def empty[TagT]: AffectedTags[TagT] = new AffectedTags[TagT](Set.empty)
}
