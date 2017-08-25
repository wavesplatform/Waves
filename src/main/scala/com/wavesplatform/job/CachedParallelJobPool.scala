package com.wavesplatform.job

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.job.CachedParallelJobPool._

import scala.concurrent.duration.FiniteDuration

class CachedParallelJobPool[ItemT, GroupIdT, JobT <: Runnable](cacheExpiration: FiniteDuration,
                                                               orig: ParallelJobPool[ItemT, GroupIdT, JobT])
                                                              (id: ItemT => String)
  extends ParallelJobPool[ItemT, GroupIdT, JobT] {

  private val added: Cache[String, Object] = CacheBuilder.newBuilder()
    .expireAfterWrite(cacheExpiration.toMillis, TimeUnit.MILLISECONDS)
    .build[String, Object]()

  override def add(item: ItemT): Unit = {
    added.get(id(item), { () =>
      orig.add(item)
      Dummy
    })
  }

  override def groupId(item: ItemT): GroupIdT = orig.groupId(item)
  override def newJob(item: ItemT): JobT = orig.newJob(item)
  override def newJobPool: JobPool[JobT] = orig.newJobPool

}

object CachedParallelJobPool {
  private val Dummy = new Object
}
