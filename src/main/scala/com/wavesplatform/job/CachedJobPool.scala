package com.wavesplatform.job

import java.util.concurrent.ConcurrentHashMap

class CachedJobPool[JobT](orig: JobPool[JobT], id: JobT => String) extends JobPool[JobT] {
  private val cache = new ConcurrentHashMap[String, Boolean]()

  override def add(job: JobT): Unit = {
    cache.computeIfAbsent(id(job), { _ =>
      orig.add(job)
      true
    })
  }

  override def shutdownNow(): Unit = orig.shutdownNow()
}
