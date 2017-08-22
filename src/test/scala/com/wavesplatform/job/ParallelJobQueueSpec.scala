package com.wavesplatform.job

import com.wavesplatform.job.ParallelJobQueueSpec._
import org.scalatest.{FreeSpec, Matchers}

class ParallelJobQueueSpec extends FreeSpec with Matchers {

  "ParallelJobQueue" - {
    "should create a missing group" in {
      var calls = 0
      val queue = new TestParallelJobQueue {
        override def newJobQueue: JobQueue[TestJob] = {
          calls += 1
          super.newJobQueue
        }
      }

      queue.enqueue("foo")
      queue.enqueue("bar")
      queue.enqueue("baz")

      calls shouldBe 2
    }

    "should enqueue a job to an according queue" in {
      var queuedJobs = List.empty[(Char, TestJob)]
      def queuedJobsOf(groupId: Char): List[TestJob] = {
        queuedJobs.collect { case (`groupId`, x) => x }
      }

      val queue = new TestParallelJobQueue {
        override def newJobQueue: JobQueue[TestJob] = {
          new TestJobQueue {
            override def enqueue(job: TestJob): Unit = {
              queuedJobs ::= groupId(job.item) -> job
              super.enqueue(job)
            }
          }
        }
      }

      queue.enqueue("foo")
      queue.enqueue("bar")
      queue.enqueue("baz")

      queuedJobsOf('f') shouldBe List(TestJob("foo"))
      queuedJobsOf('b') shouldBe List(TestJob("baz"), TestJob("bar"))
    }

    "should shutdown a group when it is done" in {
      var calls = 0

      val queue = new TestParallelJobQueue {
        override def newJobQueue: JobQueue[TestJob] = {
          new TestJobQueue {
            override def shutdownNow(): Unit = {
              calls += 1
              super.shutdownNow()
            }
          }
        }
      }

      queue.enqueue("foo")
      queue.shutdownGroup("foo")

      calls shouldBe 1
    }
  }

}

private object ParallelJobQueueSpec {

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

  class TestParallelJobQueue extends ParallelJobQueue[String, Char, TestJob] {
    override def groupId(item: String): Char = item.head
    override def newJob(item: String): TestJob = TestJob(item)
    override def newJobQueue: JobQueue[TestJob] = new TestJobQueue
  }

}
