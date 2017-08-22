package com.wavesplatform.job

import com.wavesplatform.job.DelayedJobQueueSpec.TestJob
import monix.execution.schedulers.SchedulerService
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration.DurationInt

class DelayedJobQueueSpec extends FreeSpec with Matchers {

  private val waitResponseTimeout = 500.millis
  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.singleThread("DelayedJobQueueSpec")

  "DelayedJobQueue" - {
    "should immediately start" - {
      "the first job" in {
        val queue = new DelayedJobQueue[TestJob](waitResponseTimeout)
        val job = TestJob("foo")

        queue.enqueue(job)
        Thread.sleep((0.5 * waitResponseTimeout).toMillis)

        job.started shouldBe true
      }

      "the new job if it is added after the previous one was timed out" in {
        val queue = new DelayedJobQueue[TestJob](waitResponseTimeout)
        val newJob = TestJob("bar")

        queue.enqueue(TestJob("foo"))
        Thread.sleep((1.7 * waitResponseTimeout).toMillis)

        queue.enqueue(newJob)
        Thread.sleep((0.5 * waitResponseTimeout).toMillis)

        newJob.started shouldBe true
      }
    }

    "should start a next job, when the previous one is timed out" in {
      val queue = new DelayedJobQueue[TestJob](waitResponseTimeout)
      val next = TestJob("bar")

      queue.enqueue(TestJob("foo"))
      queue.enqueue(next)
      Thread.sleep((1.5 * waitResponseTimeout).toMillis)

      next.started shouldBe true
    }

    "should not start a new job if the queue was shutdown" in {
      val queue = new DelayedJobQueue[TestJob](waitResponseTimeout)
      val newJob = TestJob("foo")

      queue.shutdownNow()
      queue.enqueue(newJob)

      newJob.started shouldBe false
    }

    "should not start a next job" - {
      "before the previous one is timed out" in {
        val queue = new DelayedJobQueue[TestJob](waitResponseTimeout)
        val nextJob = TestJob("bar")

        queue.enqueue(TestJob("foo"))
        queue.enqueue(nextJob)
        Thread.sleep((0.5 * waitResponseTimeout).toMillis)

        nextJob.started shouldBe false
      }

      "after the queue was shutdown" in {
        val queue = new DelayedJobQueue[TestJob](waitResponseTimeout)
        val nextJob = TestJob("bar")

        queue.enqueue(TestJob("foo"))
        queue.shutdownNow()
        queue.enqueue(nextJob)
        Thread.sleep((1.7 * waitResponseTimeout).toMillis)

        nextJob.started shouldBe false
      }
    }
  }

}

private object DelayedJobQueueSpec {
  case class TestJob(item: String) extends Runnable {
    @volatile var started = false

    override def run(): Unit = {
      started = true
    }
  }
}