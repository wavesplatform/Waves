package com.wavesplatform.ride.runner.requests

import cats.syntax.option.*
import com.wavesplatform.metrics.TimerExt
import com.wavesplatform.ride.runner.stats.RideRunnerStats

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NoStackTrace

trait JobScheduler[T] {
  def isEmpty: Boolean
  def add(item: T, prioritized: Boolean = false): Unit
  def addMultiple(items: Iterable[T], prioritized: Boolean = false): Unit
  def getJobs(n: Int): Seq[T]
  def getJob(): Option[T]
}

// TODO Queue with Set
class SynchronizedJobScheduler[T] private (
    prioritizedMinSize: Int,
    prioritizedItems: mutable.Set[T],
    prioritizedQueue: mutable.Queue[T],
    regularItems: mutable.Set[T],
    regularQueue: mutable.Queue[T]
) extends JobScheduler[T] {
  override def isEmpty: Boolean = regularQueue.isEmpty

  override def add(item: T, prioritized: Boolean): Unit = synchronized {
    unsafeAddOne(item, prioritized)
  }

  override def addMultiple(items: Iterable[T], prioritized: Boolean): Unit = synchronized {
    items.foreach(unsafeAddOne(_, prioritized))
  }

  private def unsafeAddOne(job: T, prioritized: Boolean): Unit =
    if (!prioritizedItems.contains(job)) {
      if (prioritized) {
        unsafeAddPrioritized(job)
        unsafeRemoveRegular(job)
      } else unsafeAddRegular(job)
    }

  // TODO
  override def getJobs(n: Int): Seq[T] = Seq.empty

  override def getJob(): Option[T] = RideRunnerStats.jobSchedulerGetJobTime.measure {
    synchronized {
      if (prioritizedQueue.isEmpty) {
        // TODO optimize
        (0 to math.min(regularQueue.size - 1, prioritizedMinSize)).foldLeft(none[T]) { case (r, _) =>
          val job = regularQueue.dequeue()
          regularItems.remove(job)
          RideRunnerStats.regularWorkSize.decrement()

          if (r.isEmpty) job.some
          else {
            unsafeAddPrioritized(job)
            r
          }
        }
      } else {
        RideRunnerStats.prioritizedWorkSize.decrement()
        Some(prioritizedQueue.dequeue().tap(prioritizedItems.remove))
      }
    }
  }

  private def unsafeAddPrioritized(job: T): Boolean = {
    val added = !prioritizedItems.contains(job)
    if (added) {
      prioritizedQueue.enqueue(job)
      prioritizedItems.add(job)
      RideRunnerStats.prioritizedWorkSize.increment()
    }
    added
  }

  private def unsafeAddRegular(job: T): Boolean = {
    val added = !regularItems.contains(job)
    if (added) {
      regularQueue.enqueue(job)
      regularItems.add(job)
      RideRunnerStats.regularWorkSize.increment()
    }
    added
  }

  private def unsafeRemoveRegular(job: T): Boolean = {
    val removed = regularItems.contains(job)
    if (removed) {
      regularQueue.removeFirst(_ == job)
      regularItems.remove(job)
      RideRunnerStats.regularWorkSize.decrement()
    }
    removed
  }
}

object SynchronizedJobScheduler {
  def apply[T](prioritizedMinSize: Int): SynchronizedJobScheduler[T] =
    new SynchronizedJobScheduler[T](
      prioritizedMinSize = prioritizedMinSize,
      prioritizedItems = mutable.Set.empty,
      prioritizedQueue = mutable.Queue.empty,
      regularItems = mutable.Set.empty,
      regularQueue = mutable.Queue.empty
    )

  case object ImpossibleCase extends RuntimeException("Impossible case") with NoStackTrace
}
