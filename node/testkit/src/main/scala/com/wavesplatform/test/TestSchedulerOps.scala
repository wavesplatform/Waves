package com.wavesplatform.test

import com.wavesplatform.utils.ScorexLogging
import monix.execution.schedulers.TestScheduler
import org.scalatest.Assertions

import scala.concurrent.duration.DurationInt

trait TestSchedulerOps { this: Assertions & ScorexLogging =>
  extension (self: TestScheduler) {
    def tickNext(label: String, failIfNoTasks: Boolean = true): Unit = {
      val before      = self.state.clock
      val closestTask = self.state.tasks.headOption
      closestTask match {
        case Some(closestTask) =>
          val time = closestTask.runsAt - before
          log.debug(s"Run $label task #${closestTask.id} ${waitStr(closestTask)}")
          self.tick(time)

        case None =>
          if (failIfNoTasks) fail(s"Run $label: no tasks in scheduler $self")
      }
    }

    def logTasks(label: String): Unit =
      if (self.state.tasks.isEmpty) log.debug(s"$label: no tasks")
      else log.debug(s"$label tasks: ${self.state.tasks.map(t => s"#${t.id} ${waitStr(t)}").mkString(", ")}")

    def waitStr(t: TestScheduler.Task): String = {
      val x = t.runsAt - self.state.clock
      if (x > 0.millis) s"in ${x.toCoarsest}"
      else "now"
    }
  }
}
