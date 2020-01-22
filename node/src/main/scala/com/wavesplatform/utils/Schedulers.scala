package com.wavesplatform.utils

import java.util.concurrent.ThreadPoolExecutor.DiscardOldestPolicy
import java.util.concurrent.{Future => JavaFuture, _}

import io.netty.util.{Timeout, Timer}
import monix.execution.schedulers.{ExecutorScheduler, SchedulerService}
import monix.execution.{ExecutionModel, Features, Scheduler, UncaughtExceptionReporter}

import scala.concurrent.duration._

/** Helper methods to create schedulers with custom DiscardPolicy */
object Schedulers {
  private trait AdaptedThreadPoolExecutorMixin { self: ThreadPoolExecutor =>
    def reportFailure(t: Throwable): Unit

    override def afterExecute(r: Runnable, t: Throwable): Unit = {
      self.afterExecute(r, t)
      var exception: Throwable = t

      if ((exception eq null) && r.isInstanceOf[JavaFuture[_]]) {
        try {
          val future = r.asInstanceOf[JavaFuture[_]]
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
    val thread = new Thread(r)
    thread.setName(name + "-" + thread.getId)
    thread.setDaemon(daemonic)
    thread.setUncaughtExceptionHandler((_: Thread, e: Throwable) => reporter.reportFailure(e))

    thread
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

  private class TimedWrapper[V](timer: Timer, timeout: FiniteDuration, delegate: RunnableScheduledFuture[V]) extends RunnableScheduledFuture[V] {
    @volatile
    private[this] var maybeScheduledTimeout     = Option.empty[Timeout]
    override def isPeriodic: Boolean            = delegate.isPeriodic
    override def getDelay(unit: TimeUnit): Long = delegate.getDelay(unit)
    override def compareTo(o: Delayed): Int     = delegate.compareTo(o)
    override def run(): Unit = {
      val workerThread = Thread.currentThread()
      maybeScheduledTimeout = Some(
        timer.newTimeout(
          (t: Timeout) =>
            if (!t.isCancelled) {
              workerThread.interrupt()
            },
          timeout.toMillis,
          MILLISECONDS
        )
      )
      delegate.run()
      maybeScheduledTimeout.foreach(_.cancel())
      maybeScheduledTimeout = None
    }
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
      maybeScheduledTimeout.foreach(_.cancel())
      delegate.cancel(mayInterruptIfRunning)
    }
    override def isCancelled: Boolean                  = delegate.isCancelled
    override def isDone: Boolean                       = delegate.isDone
    override def get(): V                              = delegate.get()
    override def get(timeout: Long, unit: TimeUnit): V = delegate.get(timeout, unit)
  }

  def timeBoundedFixedPool(
      timer: Timer,
      timeout: FiniteDuration,
      poolSize: Int,
      name: String,
      reporter: UncaughtExceptionReporter = UncaughtExceptionReporter.default,
      executionModel: ExecutionModel = ExecutionModel.Default,
      rejectedExecutionHandler: RejectedExecutionHandler = new DiscardOldestPolicy
  ): SchedulerService = {
    val factory = threadFactory(name, daemonic = true, reporter)
    val executor = new ScheduledThreadPoolExecutor(poolSize, factory, rejectedExecutionHandler) with AdaptedThreadPoolExecutorMixin {
      override def reportFailure(t: Throwable): Unit = reporter.reportFailure(t)
      override def decorateTask[V](runnable: Runnable, task: RunnableScheduledFuture[V]): RunnableScheduledFuture[V] =
        new TimedWrapper(timer, timeout, task)
      override def decorateTask[V](callable: Callable[V], task: RunnableScheduledFuture[V]): RunnableScheduledFuture[V] =
        new TimedWrapper(timer, timeout, task)
    }

    ExecutorScheduler(executor, reporter, executionModel, Features.empty)
  }

  import scala.concurrent.{Future, Promise}
  import scala.util.control.NonFatal

  // Catches InterruptedException correctly
  def executeCatchingInterruptedException[T](pool: Scheduler)(f: => T): Future[T] = {
    val promise = Promise[T]
    pool.execute { () =>
      try promise.success(f)
      catch { case e @ (NonFatal(_) | _: InterruptedException) => promise.failure(e) }
    }
    promise.future
  }
}
