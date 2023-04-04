package com.wavesplatform.ride.runner.requests

import monix.execution.Scheduler
import monix.reactive.subjects.ConcurrentSubject

class TestJobScheduler[T]()(implicit scheduler: Scheduler) extends JobScheduler[T] {
  override val jobs: ConcurrentSubject[T, T]                               = ConcurrentSubject.publish[T]
  override def add(item: T, prioritized: Boolean): Unit                    = jobs.onNext(item)
  override def addMultiple(items: Iterable[T], prioritized: Boolean): Unit = items.foreach(jobs.onNext)
  override def close(): Unit                                               = jobs.onComplete()
}
