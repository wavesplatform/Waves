package com.wavesplatform.job

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.job.CachedParallelJobQueue._

class CachedParallelJobQueue[ItemT, GroupIdT <: Object, JobT <: Runnable](val orig: ParallelJobQueue[ItemT, GroupIdT, JobT])
  extends ParallelJobQueue[ItemT, GroupIdT, JobT] {

  private val processed: Cache[GroupIdT, Object] = CacheBuilder.newBuilder()
    .expireAfterWrite(1, TimeUnit.MINUTES)
    .build[GroupIdT, Object]()

  override def enqueue(item: ItemT): Unit = {
    if (!isGroupCompleted(item)) {
      orig.enqueue(item)
    }
  }

  override def shutdownGroup(item: ItemT): Unit = {
    completeGroup(item)
    orig.shutdownGroup(item)
  }

  override def groupId(item: ItemT): GroupIdT = orig.groupId(item)
  override def newJob(item: ItemT): JobT = orig.newJob(item)
  override def newJobQueue: JobQueue[JobT] = orig.newJobQueue

  private def completeGroup(item: ItemT): Unit = {
    val id = groupId(item)
    processed.put(id, Dummy)
  }

  private def isGroupCompleted(item: ItemT): Boolean = {
    val id = groupId(item)
    Option(processed.getIfPresent(id)).isDefined
  }
}

object CachedParallelJobQueue {
  private val Dummy = new Object
}
