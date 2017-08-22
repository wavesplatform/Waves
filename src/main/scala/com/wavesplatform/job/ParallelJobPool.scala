package com.wavesplatform.job

import java.util.concurrent.ConcurrentHashMap

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
