package com.wavesplatform.concurrent

import java.util.concurrent.Semaphore

trait MayBeSemaphore {
  def limited[T](f: => T): T
}

object MayBeSemaphore {
  def apply(permits: Option[Int]): MayBeSemaphore = permits.fold[MayBeSemaphore](DummySemaphore)(new WrappedSemaphore(_))

  class WrappedSemaphore(permits: Int) extends MayBeSemaphore {
    private val available = new Semaphore(permits)
    override def limited[T](f: => T): T = {
      available.acquire()
      try f
      finally available.release()
    }
  }

  object DummySemaphore extends MayBeSemaphore {
    override def limited[T](f: => T): T = f
  }
}
