package com.wavesplatform.concurrent

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.concurrent.{Future, Promise}

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
