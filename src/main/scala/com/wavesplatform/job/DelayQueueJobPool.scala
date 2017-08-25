package com.wavesplatform.job

import monix.execution.Cancelable
import monix.execution.schedulers.SchedulerService
import scorex.utils.ScorexLogging

import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

class DelayQueueJobPool[JobT <: Runnable](delay: FiniteDuration)(implicit val scheduler: SchedulerService)
  extends JobPool[JobT] with ScorexLogging {

  private var isReady = true
  private var pending = Queue.empty[JobT]
  private var isScheduled = false
  private var timer = Cancelable.empty

  override def add(job: JobT): Unit = runInQueue {
    if (isReady) {
      if (isScheduled) pending = pending.enqueue(job)
      else {
        isScheduled = true
        job.run()
        schedule()
      }
    }
  }

  override def shutdownNow(): Unit = runInQueue {
    isReady = false
    pending = Queue.empty
    timer.cancel()
  }

  private def schedule(): Unit = {
    timer = scheduler.scheduleOnce(delay) {
      pending = pending.dequeueOption match {
        case Some((job, rest)) =>
          job.run()
          schedule()
          rest

        case _ =>
          isScheduled = false
          pending
      }
    }
  }

  private def runInQueue(f: => Unit): Unit = monix.eval.Task(f).runOnComplete {
    case Failure(e) => log.error("Failed to run in queue", e)
    case Success(_) =>
  }(scheduler)
}
