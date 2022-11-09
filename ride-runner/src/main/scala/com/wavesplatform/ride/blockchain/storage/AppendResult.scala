package com.wavesplatform.ride.blockchain.storage

import com.wavesplatform.ride.blockchain.DataKey

case class AppendResult[TagT](mayBeChangedKey: Option[DataKey], affectedTags: Set[TagT])
object AppendResult {
  def appended[TagT](changedKey: DataKey, affectedTags: Set[TagT]): AppendResult[TagT] = new AppendResult[TagT](Some(changedKey), affectedTags)
  def ignored[TagT]: AppendResult[TagT]                                                = new AppendResult[TagT](None, Set.empty)
}
