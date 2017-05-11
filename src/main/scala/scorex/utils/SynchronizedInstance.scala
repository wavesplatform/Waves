package scorex.utils

import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

import scorex.utils.SynchronizedOver._

// http://vlkan.com/blog/post/2015/09/09/enforce-locking/

object SynchronizedOver {

  sealed trait TypedLock {

    protected val instance: Lock

    def lock(): Unit = instance.lock()

    def unlock(): Unit = instance.unlock()

    def tryLock(): Boolean = instance.tryLock()
  }

  sealed class ReadLock(readLock: ReentrantReadWriteLock.ReadLock) extends TypedLock {
    override protected val instance: Lock = readLock
  }

  sealed class ReadWriteLock(readLock: ReentrantReadWriteLock.ReadLock, writeLock: ReentrantReadWriteLock.WriteLock)
    extends ReadLock(readLock) {
    override protected val instance: Lock = writeLock
  }

}

trait SynchronizedOver {

  val synchronizationToken: ReentrantReadWriteLock

  protected val readLock: ReadLock =
    new ReadLock(synchronizationToken.readLock())

  protected val readWriteLock: ReadWriteLock =
    new ReadWriteLock(synchronizationToken.readLock(), synchronizationToken.writeLock())

  protected case class Synchronized[T](private var value: T) {

    def apply()(implicit readLock: ReadLock): T = {
      validateLock(readLock, readLock, readWriteLock)
      value
    }

    def update[R](f: T => R)(implicit readWriteLock: ReadWriteLock): R = {
      validateLock(readWriteLock, readWriteLock)
      f(value)
    }

    def swap(newVal: => T)(implicit readWriteLock: ReadWriteLock): T = {
      validateLock(readWriteLock, readWriteLock)
      val oldVal = value
      value = newVal
      oldVal
    }

    private def validateLock(lock: TypedLock, allowedLocks: TypedLock*): Unit = {
      require(allowedLocks.contains(lock), "cannot be accessed from another synchronization scope")
      require(lock.tryLock(), "cannot be accessed out of a synchronization scope")
      lock.unlock()
    }

  }

  protected def read[T](body: ReadLock => T): T =
    synchronizeOperation(readLock)(body)

  protected def write[T](body: ReadWriteLock => T): T =
    synchronizeOperation(readWriteLock)(body)

  protected def synchronizeOperation[T, L <: TypedLock](lock: L)(body: L => T): T = {
    lock.lock()
    try {
      body(lock)
    }
    finally {
      lock.unlock()
    }
  }
}