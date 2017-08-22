package com.wavesplatform.job

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.job.CachedParallelJobPool._

class CachedParallelJobPool[ItemT, GroupIdT <: Object, JobT <: Runnable](val orig: ParallelJobPool[ItemT, GroupIdT, JobT])
  extends ParallelJobPool[ItemT, GroupIdT, JobT] {

  private val closed: Cache[GroupIdT, Object] = CacheBuilder.newBuilder()
    .expireAfterWrite(1, TimeUnit.MINUTES)
    .build[GroupIdT, Object]()

  override def add(item: ItemT): Unit = {
    if (!isGroupClosed(item)) {
      orig.add(item)
    }
  }

  override def shutdownPoolOf(item: ItemT): Unit = {
    val id = groupId(item)
    closed.put(id, Dummy)
    orig.shutdownPoolOf(item)
  }

  override def groupId(item: ItemT): GroupIdT = orig.groupId(item)
  override def newJob(item: ItemT): JobT = orig.newJob(item)
  override def newJobPool: JobPool[JobT] = orig.newJobPool

  private def isGroupClosed(item: ItemT): Boolean = {
    val id = groupId(item)
    Option(closed.getIfPresent(id)).isDefined
  }
}

object CachedParallelJobPool {
  private val Dummy = new Object
}
