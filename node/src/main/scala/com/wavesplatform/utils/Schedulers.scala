package com.wavesplatform.utils

import java.util.concurrent.ThreadPoolExecutor.DiscardOldestPolicy
import java.util.concurrent.{RejectedExecutionHandler, ScheduledThreadPoolExecutor, ThreadFactory}

import monix.execution.UncaughtExceptionReporter.LogExceptionsToStandardErr
import monix.execution.schedulers.{ExecutorScheduler, SchedulerService}
import monix.execution.{ExecutionModel, UncaughtExceptionReporter}

/** Helper methods to create schedulers with custom DiscardPolicy
  * We doesn't use [[monix.execution.schedulers.AdaptedThreadPoolExecutorMixin]]
  * because we will not submit Runnables to schedulers
  */
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
                   executionModel: ExecutionModel = ExecutionModel.Default,
                   rejectedExecutionHandler: RejectedExecutionHandler = new DiscardOldestPolicy): SchedulerService = {
    val factory  = threadFactory(name, daemonic = true, reporter)
    val executor = new ScheduledThreadPoolExecutor(1, factory, rejectedExecutionHandler)

    ExecutorScheduler(executor, reporter, executionModel)
  }

  def fixedPool(poolSize: Int,
                name: String,
                reporter: UncaughtExceptionReporter = LogExceptionsToStandardErr,
                executionModel: ExecutionModel = ExecutionModel.Default,
                rejectedExecutionHandler: RejectedExecutionHandler = new DiscardOldestPolicy): SchedulerService = {
    val factory  = threadFactory(name, daemonic = true, reporter)
    val executor = new ScheduledThreadPoolExecutor(poolSize, factory, rejectedExecutionHandler)

    ExecutorScheduler(executor, reporter, executionModel)
  }
}
