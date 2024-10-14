package com.wavesplatform.utils

import java.util.concurrent.locks.StampedLock

import scala.util.control.NonFatal

trait OptimisticLockable {
  private val lock = new StampedLock

  protected def readLockCond[A](getValue: => A)(shouldRecheck: A => Boolean): A = {
    def readLocked(): A = {
      val stamp = lock.readLockInterruptibly()
      try {
        getValue
      } finally {
        lock.unlockRead(stamp)
      }
    }

    val stamp = lock.tryOptimisticRead()
    try {
      val result = getValue
      if (!lock.validate(stamp) && shouldRecheck(result)) readLocked()
      else result
    } catch {
      case NonFatal(_) if !lock.validate(stamp) =>
        readLocked()
    }
  }

  protected def writeLock[A](a: => A): A = {
    val stamp = lock.writeLock()
    try {
      a
    } finally {
      lock.unlockWrite(stamp)
    }
  }
}
