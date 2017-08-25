package com.wavesplatform.job

import com.wavesplatform.job.StaleGroupsParallelJobPoolSpec._
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration.DurationInt

class StaleGroupsParallelJobPoolSpec extends FreeSpec with Matchers {

  "StaleGroupsParallelJobPool" - {
    "should add a job to an original pool" in {
      val orig = new TestParallelJobPool
      val cached = testJobPool(orig)
      cached.add("foo")
      cached.add("bar")

      orig.jobs shouldBe List("bar", "foo")
    }

    "should not add a job after it group was shutdown" in {
      val orig = new TestParallelJobPool
      val cached = testJobPool(orig)
      cached.shutdownPoolOf("bar")
      cached.add("bar")

      orig.jobs shouldBe empty
    }

    "shutdownGroup should call an original shutdownGroup" in {
      val orig = new TestParallelJobPool
      val cached = testJobPool(orig)
      cached.shutdownPoolOf("foo")

      orig.shutdownNowCalls.size shouldBe 1
      orig.shutdownNowCalls("foo") shouldBe 1
    }
  }

  private def testJobPool(orig: TestParallelJobPool) = new StaleGroupsParallelJobPool(1.minute, orig)

}

private object StaleGroupsParallelJobPoolSpec {

  case class TestJob(item: String) extends Runnable {
    override def run(): Unit = {}
  }

  class TestJobPool extends JobPool[TestJob] {
    override def add(job: TestJob): Unit = {}
    override def shutdownNow(): Unit = {}
  }

  class TestParallelJobPool extends ParallelJobPool[String, String, TestJob] {
    var jobs: List[String] = List.empty
    var shutdownNowCalls: Map[String, Int] = Map.empty.withDefaultValue(0)

    override def add(job: String): Unit = {
      jobs ::= job
      super.add(job)
    }

    override def shutdownPoolOf(item: String): Unit = {
      shutdownNowCalls = shutdownNowCalls.updated(item, shutdownNowCalls(item) + 1)
      super.shutdownPoolOf(item)
    }

    override def groupId(item: String): String = item
    override def newJob(item: String): TestJob = TestJob(item)
    override def newJobPool: JobPool[TestJob] = new TestJobPool
  }

}
