package com.wavesplatform.job

import com.wavesplatform.job.CachedParallelJobPoolSpec._
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration.DurationInt

class CachedParallelJobPoolSpec extends FreeSpec with Matchers {

  "CachedParallelJobPool" - {
    "should add a job with an unique id to an original pool" in {
      val orig = new TestParallelJobPool
      val cached = testJobPool(orig)
      cached.add("foo")
      cached.add("bar")

      orig.items shouldBe List("bar", "foo")
    }

    "should not add a job with the same id to an original pool" in {
      val orig = new TestParallelJobPool
      val cached = testJobPool(orig)
      cached.add("bar")
      cached.add("bar")

      orig.items shouldBe List("bar")
    }
  }

  private def testJobPool(orig: TestParallelJobPool) = new CachedParallelJobPool(1.minute, orig)(identity)

}

private object CachedParallelJobPoolSpec {

  case class TestJob(item: String) extends Runnable {
    override def run(): Unit = {}
  }

  class TestJobPool extends JobPool[TestJob] {
    override def add(job: TestJob): Unit = {}
    override def shutdownNow(): Unit = {}
  }

  class TestParallelJobPool extends ParallelJobPool[String, String, TestJob] {
    var items: List[String] = List.empty

    override def add(item: String): Unit = {
      items ::= item
      super.add(item)
    }

    override def groupId(item: String): String = item
    override def newJob(item: String): TestJob = TestJob(item)
    override def newJobPool: JobPool[TestJob] = new TestJobPool
  }

}
