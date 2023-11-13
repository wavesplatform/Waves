package com.wavesplatform

import com.wavesplatform.utils.ScorexLogging
import monix.execution.schedulers.TestScheduler

import java.util.concurrent.TimeUnit

trait HasMonixHelpers { this: ScorexLogging =>
  def dumpTasks(label: String)(implicit testScheduler: TestScheduler): Unit = logWithClock(s"$label: $dumpedTasks")
  def dumpedTasks(implicit testScheduler: TestScheduler): String = {
    val tasks    = testScheduler.state.tasks
    val currTime = testScheduler.state.clock
    tasks
      .map { x =>
        val runIn = x.runsAt - currTime
        s"$x in $runIn"
      }
      .mkString("\n")
  }

  def logWithClock(s: String)(implicit testScheduler: TestScheduler): Unit = log.info(s"[${testScheduler.clockMonotonic(TimeUnit.MILLISECONDS)}] $s")
}
