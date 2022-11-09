package com.wavesplatform.storage.actions

import com.wavesplatform.storage.Storage.DataKey

case class AppendResult[TagT](mayBeChangedKey: Option[DataKey], affectedTags: Set[TagT])
object AppendResult {
  def appended[TagT](changedKey: DataKey, affectedTags: Set[TagT]): AppendResult[TagT] = new AppendResult[TagT](Some(changedKey), affectedTags)
  def ignored[TagT]: AppendResult[TagT]                                                = new AppendResult[TagT](None, Set.empty)
}
