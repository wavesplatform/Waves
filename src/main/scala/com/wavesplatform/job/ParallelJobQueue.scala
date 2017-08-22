package com.wavesplatform.job

import java.util.concurrent.ConcurrentHashMap

trait ParallelJobQueue[ItemT, GroupIdT, JobT] {
  private val jobs = new ConcurrentHashMap[GroupIdT, JobQueue[JobT]]()

  def enqueue(item: ItemT): Unit = {
    val id = groupId(item)
    jobs
      .computeIfAbsent(id, _ => newJobQueue)
      .enqueue(newJob(item))
  }

  def shutdownGroup(item: ItemT): Unit = {
    val id = groupId(item)
    Option(jobs.remove(id)).foreach { queue =>
      queue.shutdownNow()
    }
  }

  def groupId(item: ItemT): GroupIdT
  def newJob(item: ItemT): JobT
  def newJobQueue: JobQueue[JobT]
}
