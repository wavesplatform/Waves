package com.wavesplatform.utils

import java.util.concurrent.ThreadPoolExecutor.DiscardOldestPolicy
import java.util.concurrent._

import monix.execution.schedulers.{ExecutorScheduler, SchedulerService}
import monix.execution.{ExecutionModel, Features, UncaughtExceptionReporter}

/** Helper methods to create schedulers with custom DiscardPolicy */
object Schedulers {
  private trait AdaptedThreadPoolExecutorMixin { self: ThreadPoolExecutor =>
    def reportFailure(t: Throwable): Unit

    override def afterExecute(r: Runnable, t: Throwable): Unit = {
      self.afterExecute(r, t)
      var exception: Throwable = t

      if ((exception eq null) && r.isInstanceOf[Future[_]]) {
        try {
          val future = r.asInstanceOf[Future[_]]
          if (future.isDone) future.get()
        } catch {
          case ex: ExecutionException =>
            exception = ex.getCause
          case _: InterruptedException =>
            // ignore/reset
            Thread.currentThread().interrupt()
          case _: CancellationException =>
            () // ignore
        }
      }

      if (exception ne null) reportFailure(exception)
    }
  }

  private[this] def threadFactory(name: String, daemonic: Boolean, reporter: UncaughtExceptionReporter): ThreadFactory = { r: Runnable =>
    {
      val thread = new Thread(r)
      thread.setName(name + "-" + thread.getId)
      thread.setDaemon(daemonic)
      thread.setUncaughtExceptionHandler((_: Thread, e: Throwable) => reporter.reportFailure(e))

      thread
    }
  }

  def singleThread(
      name: String,
      reporter: UncaughtExceptionReporter = UncaughtExceptionReporter.default,
      executionModel: ExecutionModel = ExecutionModel.Default,
      rejectedExecutionHandler: RejectedExecutionHandler = new DiscardOldestPolicy
  ): SchedulerService = {
    val factory = threadFactory(name, daemonic = true, reporter)
    val executor = new ScheduledThreadPoolExecutor(1, factory, rejectedExecutionHandler) with AdaptedThreadPoolExecutorMixin {
      override def reportFailure(t: Throwable): Unit = reporter.reportFailure(t)
    }

    ExecutorScheduler(executor, reporter, executionModel, Features.empty)
  }

  def fixedPool(
      poolSize: Int,
      name: String,
      reporter: UncaughtExceptionReporter = UncaughtExceptionReporter.default,
      executionModel: ExecutionModel = ExecutionModel.Default,
      rejectedExecutionHandler: RejectedExecutionHandler = new DiscardOldestPolicy
  ): SchedulerService = {
    val factory = threadFactory(name, daemonic = true, reporter)
    val executor = new ScheduledThreadPoolExecutor(poolSize, factory, rejectedExecutionHandler) with AdaptedThreadPoolExecutorMixin {
      override def reportFailure(t: Throwable): Unit = reporter.reportFailure(t)
    }

    ExecutorScheduler(executor, reporter, executionModel, Features.empty)
  }
}
