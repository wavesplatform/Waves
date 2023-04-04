package com.wavesplatform.ride.runner.requests

import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.collection.mutable

trait JobScheduler[T] extends AutoCloseable {
  def add(item: T, prioritized: Boolean = false): Unit
  def addMultiple(items: Iterable[T], prioritized: Boolean = false): Unit
  val jobs: Observable[T]
}

class SynchronizedJobScheduler[T]()(implicit val scheduler: Scheduler) extends JobScheduler[T] {
  private val prioritizedQueue = ConcurrentSubject.publish[T]
  private val prioritizedItems = mutable.Set.empty[T]

  private val regularQueue = ConcurrentSubject.publish[T]
  private val regularItems = mutable.Set.empty[T]

  override val jobs: Observable[T] = Observable
    .mergePrioritizedList(
      // TODO buffer with drop new?
      1 -> prioritizedQueue.doOnNextAck((x, _) => Task(prioritizedItems.remove(x))),
      0 -> regularQueue.doOnNextAck((x, _) => Task(regularItems.remove(x)))
    )

  override def add(item: T, prioritized: Boolean): Unit = synchronized {
    unsafeAdd(item, prioritized)
  }

  override def addMultiple(items: Iterable[T], prioritized: Boolean): Unit = synchronized {
    items.foreach(unsafeAdd(_, prioritized))
  }

  private def unsafeAdd(item: T, prioritized: Boolean): Unit = if (!prioritizedItems.contains(item)) {
    if (prioritized) {
      prioritizedItems.add(item)
      prioritizedQueue.onNext(item)
    } else {
      regularItems.add(item)
      regularQueue.onNext(item)
    }
  }

  override def close(): Unit = {
    prioritizedQueue.onComplete()
    regularQueue.onComplete()
  }
}
