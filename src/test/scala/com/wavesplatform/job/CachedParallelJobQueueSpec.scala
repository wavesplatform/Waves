package com.wavesplatform.job

import com.wavesplatform.job.CachedParallelJobQueueSpec._
import org.scalatest.{FreeSpec, Matchers}

class CachedParallelJobQueueSpec extends FreeSpec with Matchers {

  "CachedParallelJobQueue" - {
    "should enqueue a job to an original queue" in {
      var enqueued = List.empty[String]
      val orig = new TestParallelJobQueue {
        override def enqueue(item: String): Unit = {
          enqueued ::= item
          super.enqueue(item)
        }
      }

      val cached = new CachedParallelJobQueue(orig)
      cached.enqueue("foo")
      cached.enqueue("bar")

      enqueued shouldBe List("bar", "foo")
    }

    "should not enqueue a job after its group was shutdown" in {
      var calls = 0
      val orig = new TestParallelJobQueue {
        override def enqueue(item: String): Unit = {
          calls += 1
          super.enqueue(item)
        }
      }

      val cached = new CachedParallelJobQueue(orig)
      cached.shutdownGroup("bar")
      cached.enqueue("bar")

      calls shouldBe 0
    }

    "shutdownGroup should call an original shutdownGroup" in {
      var calls = 0
      val orig = new TestParallelJobQueue {
        override def shutdownGroup(item: String): Unit = {
          calls += 1
          super.shutdownGroup(item)
        }
      }

      val cached = new CachedParallelJobQueue(orig)
      cached.shutdownGroup("foo")

      calls shouldBe 1
    }
  }

}

private object CachedParallelJobQueueSpec {

  case class TestJob(item: String) extends Runnable {
    override def run(): Unit = {}
  }

  class TestJobQueue extends JobQueue[TestJob] {
    override def enqueue(job: TestJob): Unit = {}
    override def shutdownNow(): Unit = {}
  }

  class TestParallelJobQueue extends ParallelJobQueue[String, String, TestJob] {
    override def groupId(item: String): String = item
    override def newJob(item: String): TestJob = TestJob(item)
    override def newJobQueue: JobQueue[TestJob] = new TestJobQueue
  }

}
