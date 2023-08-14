package com.wavesplatform.ride.runner.caches

import com.wavesplatform.utils.OptimisticLockable

class HeightTagsStorage[TagT](source: => Int) extends OptimisticLockable {
  private var tags = Set.empty[TagT]

  def addDependent(tagT: TagT): Unit = tags += tagT

  def get(tag: TagT): Int = writeLock {
    tags += tag
    source
  }

  def affectedTags: AffectedTags[TagT] = readLockCond(AffectedTags(tags))(_ => false)
}
