package com.wavesplatform.concurrent

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.function.UnaryOperator

import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import monix.execution.{Cancelable, Scheduler}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

class TimeoutedFutureSemaphore {
  private val completionPromise = new AtomicReference(Promise.successful[Unit](()))

  private implicit val DefaultScheduler: SchedulerService = Scheduler.singleThread("future-semaphore")
  private var counter = 0
  private var timer: Option[Cancelable] = None

  def increment(): Unit = Task {
    counter += 1
    completionPromise.updateAndGet(new UnaryOperator[Promise[Unit]] {
      override def apply(t: Promise[Unit]) = {
        if (t.isCompleted) Promise[Unit]()
        else t
      }
    })

    timer.foreach(_.cancel())
    timer = Some(DefaultScheduler.scheduleOnce(3.seconds) {
      counter = 0
      completionPromise.get.success(())
    })
  }.runAsync

  def decrement(): Unit = Task {
    counter -= 1
    if (counter == 0) {
      timer.foreach(_.cancel())
      completionPromise.get.success(())
    }
  }.runAsync

  def completion: Future[Unit] = completionPromise.get.future
}

class FutureSemaphore {
  private val counter = new AtomicInteger(0)
  private val completionPromise = new AtomicReference(Promise.successful[Unit](()))

  def increment(): Unit = {
    if (counter.getAndIncrement() == 0) {
      completionPromise.set(Promise[Unit]())
    }
  }

  def decrement(): Unit = {
    if (counter.decrementAndGet() == 0) {
      completionPromise.get().trySuccess(())
    }
  }

  def completion: Future[Unit] = completionPromise.get.future
}
