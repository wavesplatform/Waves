package com.wavesplatform.riderunner

import cats.syntax.option.*
import com.wavesplatform.riderunner.SynchronizedJobScheduler.ImpossibleCase
import com.wavesplatform.riderunner.app.RideRunnerMetrics.*

import scala.collection.mutable
import scala.util.control.NoStackTrace

trait JobScheduler[T] {
  def prioritize(item: T): Unit
  def prioritize(items: Iterable[T]): Unit
  def getJob(): T
}

// TODO improve
class SynchronizedJobScheduler[T] private(
    prioritizedMinSize: Int,
    prioritizedQueue: mutable.Queue[T],
    allQueue: mutable.Queue[T]
) extends JobScheduler[T] {
  require(allQueue.size >= prioritizedMinSize, s"A number of jobs should >= prioritizedMinSize=$prioritizedMinSize")

  override def prioritize(item: T): Unit = jobSchedulerPrioritizeTime.measure {
    synchronized(unsafePrioritizeOne(item))
  }

  override def prioritize(items: Iterable[T]): Unit = jobSchedulerPrioritizeManyTime.measure {
    synchronized {
      items.foreach(unsafePrioritizeOne)
    }
  }

  private def unsafePrioritizeOne(item: T): Unit = {
    if (allQueue.removeFirst(_ == item).isEmpty) allWorkSize.increment()
    allQueue.enqueue(item) // Move to the end

    prioritizedQueue.enqueue(item)
    prioritizedWorkSize.increment()
  }

  override def getJob(): T = jobSchedulerGetJobTime.measure {
    synchronized {
      if (prioritizedQueue.isEmpty) {
        prioritizedWorkSize.increment(prioritizedMinSize)
        (0 to prioritizedMinSize)
          .foldLeft(none[T]) { case (r, _) =>
            val job = allQueue.dequeue()
            allQueue.enqueue(job) // Move to the end

            if (r.isEmpty) job.some
            else {
              prioritizedQueue.enqueue(job)
              r
            }
          }
          .getOrElse(throw ImpossibleCase)
      } else {
        prioritizedWorkSize.decrement()
        prioritizedQueue.dequeue()
      }
    }
  }
}

object SynchronizedJobScheduler {
  def apply[T](prioritizedMinSize: Int, knownJobs: Iterable[T]): SynchronizedJobScheduler[T] = {
    val (prioritized, rest) = knownJobs.splitAt(prioritizedMinSize)
    new SynchronizedJobScheduler[T](
      prioritizedMinSize = prioritizedMinSize,
      prioritizedQueue = prioritized.to(mutable.Queue),
      allQueue = rest.to(mutable.Queue)
    )
  }

  case object ImpossibleCase extends RuntimeException("Impossible case") with NoStackTrace
}
