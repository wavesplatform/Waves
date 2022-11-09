package com.wavesplatform.storage.actions

import com.wavesplatform.storage.Storage.DataKey

/** @param mayBeUncertainKey
  *   We will request the data from the Node if Some
  */
case class RollbackResult[TagT](mayBeUncertainKey: Option[DataKey], affectedTags: Set[TagT])
object RollbackResult {
  def uncertain[TagT](uncertainKey: DataKey, affectedTags: Set[TagT]): RollbackResult[TagT] =
    new RollbackResult[TagT](Some(uncertainKey), affectedTags)
  def ignored[TagT]: RollbackResult[TagT]                             = new RollbackResult[TagT](None, Set.empty)
  def rolledBack[TagT](affectedTags: Set[TagT]): RollbackResult[TagT] = new RollbackResult[TagT](None, affectedTags)
}
