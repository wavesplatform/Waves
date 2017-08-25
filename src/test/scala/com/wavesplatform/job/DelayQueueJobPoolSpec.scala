package com.wavesplatform.job

import com.wavesplatform.job.DelayQueueJobPoolSpec.TestJob
import monix.execution.schedulers.SchedulerService
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration.DurationInt

class DelayQueueJobPoolSpec extends FreeSpec with Matchers {

  private val delay = 500.millis
  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.singleThread("DelayQueueJobPoolSpec")

  "DelayQueueJobPool" - {
    "should immediately start" - {
      "the first job" in {
        val pool = new DelayQueueJobPool[TestJob](delay)
        val job = TestJob("foo")

        pool.add(job)
        Thread.sleep((0.5 * delay).toMillis)

        job.started shouldBe true
      }

      "the new job if it is added after the previous one was timed out" in {
        val pool = new DelayQueueJobPool[TestJob](delay)
        val newJob = TestJob("bar")

        pool.add(TestJob("foo"))
        Thread.sleep((1.7 * delay).toMillis)

        pool.add(newJob)
        Thread.sleep((0.5 * delay).toMillis)

        newJob.started shouldBe true
      }
    }

    "should start a next job, when the previous one is timed out" in {
      val pool = new DelayQueueJobPool[TestJob](delay)
      val next = TestJob("bar")

      pool.add(TestJob("foo"))
      pool.add(next)
      Thread.sleep((1.5 * delay).toMillis)

      next.started shouldBe true
    }

    "should not start a new job if the pool was shutdown" in {
      val pool = new DelayQueueJobPool[TestJob](delay)
      val newJob = TestJob("foo")

      pool.shutdownNow()
      pool.add(newJob)

      newJob.started shouldBe false
    }

    "should not start a next job" - {
      "before the previous one is timed out" in {
        val pool = new DelayQueueJobPool[TestJob](delay)
        val nextJob = TestJob("bar")

        pool.add(TestJob("foo"))
        pool.add(nextJob)
        Thread.sleep((0.5 * delay).toMillis)

        nextJob.started shouldBe false
      }

      "after the pool was shutdown" in {
        val pool = new DelayQueueJobPool[TestJob](delay)
        val nextJob = TestJob("bar")

        pool.add(TestJob("foo"))
        pool.shutdownNow()
        pool.add(nextJob)
        Thread.sleep((1.7 * delay).toMillis)

        nextJob.started shouldBe false
      }
    }
  }

}

private object DelayQueueJobPoolSpec {
  case class TestJob(item: String) extends Runnable {
    @volatile var started = false

    override def run(): Unit = {
      started = true
    }
  }
}