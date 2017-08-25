package com.wavesplatform.job

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.duration.FiniteDuration

trait ParallelJobPool[ItemT, GroupIdT, JobT] {
  private val pools = new ConcurrentHashMap[GroupIdT, JobPool[JobT]]()

  def add(item: ItemT): Unit = {
    val id = groupId(item)
    pools
      .computeIfAbsent(id, _ => newJobPool)
      .add(newJob(item))
  }

  def shutdownPoolOf(item: ItemT): Unit = {
    val id = groupId(item)
    Option(pools.remove(id)).foreach { pool =>
      pool.shutdownNow()
    }
  }

  def groupId(item: ItemT): GroupIdT
  def newJob(item: ItemT): JobT
  def newJobPool: JobPool[JobT]
}

object ParallelJobPool {
  def cached[ItemT, GroupIdT, JobT <: Runnable](cacheExpiration: FiniteDuration,
                                                orig: ParallelJobPool[ItemT, GroupIdT, JobT])
                                               (id: ItemT => String)
  : ParallelJobPool[ItemT, GroupIdT, JobT] = new CachedParallelJobPool(cacheExpiration, orig)(id)

  def ignoreStaleGroups[ItemT, GroupIdT <: Object, JobT <: Runnable](cacheExpiration: FiniteDuration,
                                                                     orig: ParallelJobPool[ItemT, GroupIdT, JobT])
  : ParallelJobPool[ItemT, GroupIdT, JobT] = new StaleGroupsParallelJobPool(cacheExpiration, orig)
}
