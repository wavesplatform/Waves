package com.wavesplatform.job

import com.wavesplatform.job.CachedJobPoolSpec._
import org.scalatest.{FreeSpec, Matchers}

class CachedJobPoolSpec extends FreeSpec with Matchers {

  "CachedJobPool" - {
    "should add a job with an unique id to an original pool" in {
      val orig = new TestJobPool
      val cached = new CachedJobPool[TestJob](orig, id = _.item.head.toString)
      cached.add(TestJob("foo"))
      cached.add(TestJob("bar"))

      orig.jobs shouldBe List(TestJob("bar"), TestJob("foo"))
    }

    "should not add a job with the same id to an original pool" in {
      val orig = new TestJobPool
      val cached = new CachedJobPool[TestJob](orig, id = _.item.head.toString)
      cached.add(TestJob("bar"))
      cached.add(TestJob("baz"))

      orig.jobs shouldBe List(TestJob("bar"))
    }

    "shutdownNow should call an original shutdownNow" in {
      val orig = new TestJobPool
      val cached = new CachedJobPool[TestJob](orig, id = _.item.head.toString)
      cached.shutdownNow()

      orig.shutdownNowCalls shouldBe 1
    }
  }

}

private object CachedJobPoolSpec {

  case class TestJob(item: String) extends Runnable {
    override def run(): Unit = {}
  }

  class TestJobPool extends JobPool[TestJob] {
    var jobs: List[TestJob] = List.empty
    var shutdownNowCalls = 0

    override def add(job: TestJob): Unit = {
      jobs ::= job
    }

    override def shutdownNow(): Unit = {
      shutdownNowCalls += 1
    }
  }

}
