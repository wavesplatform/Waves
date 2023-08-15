package com.wavesplatform.ride.runner.requests

import com.wavesplatform.ride.runner.stats.RideRunnerStats
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import java.util.concurrent.ConcurrentHashMap

trait JobScheduler[T] extends AutoCloseable {
  def add(item: T, prioritized: Boolean = false): Unit
  def addMultiple(items: Iterable[T], prioritized: Boolean = false): Unit
  val jobs: Observable[T]
}

class SynchronizedJobScheduler[T]()(implicit val scheduler: Scheduler) extends JobScheduler[T] {
  private val prioritizedQueue = ConcurrentSubject.publish[T]
  private val prioritizedItems = ConcurrentHashMap.newKeySet[T]()

  private val regularQueue = ConcurrentSubject.publish[T]
  private val regularItems = ConcurrentHashMap.newKeySet[T]()

  override val jobs: Observable[T] = Observable
    .mergePrioritizedList(
      1 -> prioritizedQueue.doOnNextAck((x, _) => Task(prioritizedItems.remove(x))),
      // There could be used a buffer with timeout in a case of large queue and/or slow updates
      0 -> regularQueue.doOnNextAck((x, _) => Task(regularItems.remove(x)))
    )

  override def add(item: T, prioritized: Boolean): Unit = {
    addOne(item, prioritized)
    updateStats(prioritized)
  }

  override def addMultiple(items: Iterable[T], prioritized: Boolean): Unit = {
    items.foreach(addOne(_, prioritized))
    updateStats(prioritized)
  }

  private def addOne(item: T, prioritized: Boolean): Unit = if (!prioritizedItems.contains(item)) {
    if (prioritized) {
      prioritizedItems.add(item)
      prioritizedQueue.onNext(item)
    } else if (!regularItems.contains(item)) {
      regularItems.add(item)
      regularQueue.onNext(item)
    }
  }

  private def updateStats(prioritized: Boolean): Unit =
    if (prioritized) RideRunnerStats.prioritizedJobSize.update(prioritizedItems.size.toDouble)
    else RideRunnerStats.regularJobSize.update(regularItems.size.toDouble)

  override def close(): Unit = {
    prioritizedQueue.onComplete()
    regularQueue.onComplete()
  }
}
