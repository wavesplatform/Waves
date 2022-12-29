package com.wavesplatform.storage.actions

case class AffectedTags[TagT](xs: Set[TagT])
object AffectedTags {
  def empty[TagT]: AffectedTags[TagT] = new AffectedTags[TagT](Set.empty)
}
