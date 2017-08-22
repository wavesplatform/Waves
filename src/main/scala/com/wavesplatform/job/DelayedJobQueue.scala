package com.wavesplatform.job

import monix.execution.Cancelable
import monix.execution.schedulers.SchedulerService
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

class DelayedJobQueue[JobT <: Runnable](delay: FiniteDuration)(implicit val scheduler: SchedulerService)
  extends JobQueue[JobT] with ScorexLogging {

  private var isReady = true
  private var pending = List.empty[JobT]
  private var isScheduled = false
  private var timer = Cancelable.empty

  override def enqueue(job: JobT): Unit = runInQueue {
    if (isReady) {
      if (isScheduled) pending ::= job
      else {
        isScheduled = true
        job.run()
        schedule()
      }
    }
  }

  override def shutdownNow(): Unit = runInQueue {
    isReady = false
    pending = List.empty
    timer.cancel()
  }

  private def schedule(): Unit = {
    timer = scheduler.scheduleOnce(delay) {
      pending match {
        case job :: rest =>
          pending = rest
          job.run()
          schedule()

        case _ =>
          isScheduled = false
      }
    }
  }

  private def runInQueue(f: => Unit): Unit = monix.eval.Task(f).runOnComplete {
    case Failure(e) => log.error("Failed to run in queue", e)
    case Success(_) =>
  }(scheduler)
}
