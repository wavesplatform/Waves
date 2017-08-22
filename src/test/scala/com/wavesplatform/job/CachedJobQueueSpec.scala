package com.wavesplatform.job

import com.wavesplatform.job.ParallelJobQueueSpec._
import org.scalatest.{FreeSpec, Matchers}

class CachedJobQueueSpec extends FreeSpec with Matchers {

  "CachedJobQueue" - {
    "should enqueue a job with unique id to an original queue" in {
      val orig = new TestJobQueue
      val cached = new CachedJobQueue[TestJob](orig, id = _.item.head.toString)
      cached.enqueue(TestJob("foo"))
      cached.enqueue(TestJob("bar"))

      orig.jobs shouldBe List(TestJob("bar"), TestJob("foo"))
    }

    "should not enqueue a job with same id to an original queue" in {
      val orig = new TestJobQueue
      val cached = new CachedJobQueue[TestJob](orig, id = _.item.head.toString)
      cached.enqueue(TestJob("bar"))
      cached.enqueue(TestJob("baz"))

      orig.jobs shouldBe List(TestJob("bar"))
    }

    "shutdownNow should call an original shutdownNow" in {
      var calls = 0
      val orig = new TestJobQueue {
        override def shutdownNow(): Unit = {
          calls += 1
        }
      }

      val cached = new CachedJobQueue[TestJob](orig, id = _.item.head.toString)
      cached.shutdownNow()

      calls shouldBe 1
    }
  }

}

private object CachedJobQueueSpec {

  case class TestJob(item: String) extends Runnable {
    override def run(): Unit = {}
  }

  class TestJobQueue extends JobQueue[TestJob] {
    var jobs: List[TestJob] = List.empty

    override def enqueue(job: TestJob): Unit = {
      jobs ::= job
    }

    override def shutdownNow(): Unit = {}
  }

}
