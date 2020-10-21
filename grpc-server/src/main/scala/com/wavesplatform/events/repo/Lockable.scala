package com.wavesplatform.events.repo

import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}

trait Lockable {
  private[this] val lock: ReadWriteLock = new ReentrantReadWriteLock()

  protected def readLock[A](a: => A): A = {
    lock.readLock().lock()
    try {
      a
    } finally {
      lock.readLock().unlock()
    }
  }

  protected def writeLock[A](a: => A): A = {
    lock.writeLock().lock()
    try {
      a
    } finally {
      lock.writeLock().unlock()
    }
  }
}
