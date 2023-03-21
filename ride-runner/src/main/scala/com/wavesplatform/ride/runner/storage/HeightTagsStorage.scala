package com.wavesplatform.ride.runner.storage

import com.wavesplatform.utils.OptimisticLockable

class HeightTagsStorage[TagT](source: => Int) extends OptimisticLockable {
  private var tags = Set.empty[TagT]

  def get(tag: TagT): Int = writeLock {
    tags += tag
    source
  }

  def affectedTags: AffectedTags[TagT] = readLockCond(AffectedTags(tags))(_ => false)
}
