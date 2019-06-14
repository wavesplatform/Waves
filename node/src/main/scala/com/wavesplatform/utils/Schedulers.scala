package com.wavesplatform.utils

import java.util.concurrent.ThreadPoolExecutor.DiscardOldestPolicy
import java.util.concurrent.{ScheduledThreadPoolExecutor, ThreadFactory}

import monix.execution.UncaughtExceptionReporter.LogExceptionsToStandardErr
import monix.execution.schedulers.{ExecutorScheduler, SchedulerService}
import monix.execution.{ExecutionModel, UncaughtExceptionReporter}

object Schedulers {

  private[this] def threadFactory(name: String, daemonic: Boolean, reporter: UncaughtExceptionReporter): ThreadFactory = { r: Runnable =>
    {
      val thread = new Thread(r)
      thread.setName(name + "-" + thread.getId)
      thread.setDaemon(daemonic)
      thread.setUncaughtExceptionHandler((_: Thread, e: Throwable) => reporter.reportFailure(e))

      thread
    }
  }

  def singleThread(name: String,
                   reporter: UncaughtExceptionReporter = LogExceptionsToStandardErr,
                   executionModel: ExecutionModel = ExecutionModel.Default): SchedulerService = {
    val factory  = threadFactory(name, daemonic = true, reporter)
    val executor = new ScheduledThreadPoolExecutor(1, factory, new DiscardOldestPolicy)

    ExecutorScheduler(executor, reporter, executionModel)
  }

  def fixedPool(poolSize: Int,
                name: String,
                reporter: UncaughtExceptionReporter = LogExceptionsToStandardErr,
                executionModel: ExecutionModel = ExecutionModel.Default): SchedulerService = {
    val factory  = threadFactory(name, daemonic = true, reporter)
    val executor = new ScheduledThreadPoolExecutor(poolSize, factory, new DiscardOldestPolicy)

    ExecutorScheduler(executor, reporter, executionModel)
  }
}
