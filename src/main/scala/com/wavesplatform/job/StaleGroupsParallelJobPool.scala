package com.wavesplatform.job

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.job.StaleGroupsParallelJobPool._

import scala.concurrent.duration.FiniteDuration

class StaleGroupsParallelJobPool[ItemT, GroupIdT <: Object, JobT <: Runnable](cacheExpiration: FiniteDuration,
                                                                              orig: ParallelJobPool[ItemT, GroupIdT, JobT])
  extends ParallelJobPool[ItemT, GroupIdT, JobT] {

  private val closed: Cache[GroupIdT, Object] = CacheBuilder.newBuilder()
    .expireAfterWrite(cacheExpiration.toMillis, TimeUnit.MILLISECONDS)
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

object StaleGroupsParallelJobPool {
  private val Dummy = new Object
}
