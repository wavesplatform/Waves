package com.wavesplatform.job

import java.util.concurrent.ConcurrentHashMap

class CachedJobQueue[JobT](orig: JobQueue[JobT], id: JobT => String) extends JobQueue[JobT] {
  private val cache = new ConcurrentHashMap[String, Boolean]()

  override def enqueue(job: JobT): Unit = {
    cache.computeIfAbsent(id(job), { _ =>
      orig.enqueue(job)
      true
    })
  }

  override def shutdownNow(): Unit = orig.shutdownNow()
}
