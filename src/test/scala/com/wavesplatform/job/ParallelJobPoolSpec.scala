package com.wavesplatform.job

import com.wavesplatform.job.ParallelJobPoolSpec._
import org.scalatest.{FreeSpec, Matchers}

class ParallelJobPoolSpec extends FreeSpec with Matchers {

  "ParallelJobPool" - {
    "should create a missing group" in {
      val pool = new TestParallelJobPool
      pool.add("foo")
      pool.add("bar")
      pool.add("baz")

      pool.jobPools.size shouldBe 2
    }

    "should add a job to an according pool" in {
      val pool = new TestParallelJobPool
      pool.add("foo")
      pool.add("bar")
      pool.add("baz")

      pool.jobPools.map(_.jobs).toSet shouldBe Set(
        List(TestJob("foo")),
        List(TestJob("baz"), TestJob("bar"))
      )
    }

    "should shutdown a group when it is done" in {
      val pool = new TestParallelJobPool
      pool.shutdownPoolOf("foo")

      pool.shutdownNowCalls.size shouldBe 1
      pool.shutdownNowCalls("foo") shouldBe 1
    }
  }

}

private object ParallelJobPoolSpec {

  case class TestJob(item: String) extends Runnable {
    override def run(): Unit = {}
  }

  class TestJobPool extends JobPool[TestJob] {
    var jobs: List[TestJob] = List.empty

    override def add(job: TestJob): Unit = {
      jobs ::= job
    }

    override def shutdownNow(): Unit = {}
  }

  class TestParallelJobPool extends ParallelJobPool[String, Char, TestJob] {
    var jobPools: List[TestJobPool] = List.empty
    var shutdownNowCalls: Map[String, Int] = Map.empty.withDefaultValue(0)

    override def shutdownPoolOf(item: String): Unit = {
      shutdownNowCalls = shutdownNowCalls.updated(item, shutdownNowCalls(item) + 1)
      super.shutdownPoolOf(item)
    }

    override def groupId(item: String): Char = item.head
    override def newJob(item: String): TestJob = TestJob(item)

    override def newJobPool: JobPool[TestJob] = {
      val r = new TestJobPool
      jobPools ::= r
      r
    }
  }

}
