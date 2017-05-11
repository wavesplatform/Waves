package scorex.utils

import java.util.concurrent.locks.{Lock, ReentrantReadWriteLock}

import scorex.utils.Synchronized.{ReadLock, _}

// http://vlkan.com/blog/post/2015/09/09/enforce-locking/

object Synchronized {

  sealed trait TypedLock {

    def lock(): Unit

    def unlock(): Unit

    def tryLock(): Boolean
  }

  sealed class ReadLock(rwl: ReentrantReadWriteLock) extends TypedLock {

    override def lock(): Unit = rwl.readLock().lock()

    override def unlock(): Unit = rwl.readLock().unlock()

    override def tryLock(): Boolean = rwl.readLock().tryLock()
  }

  sealed class WriteLock(rwl: ReentrantReadWriteLock) extends ReadLock(rwl) {
    override def lock(): Unit = rwl.writeLock.lock()

    override def unlock(): Unit = rwl.writeLock.unlock()

    override def tryLock(): Boolean = rwl.writeLock.tryLock()
  }

}

trait Synchronized {

  def synchronizationToken: ReentrantReadWriteLock

  protected val instanceReadLock: ReadLock = new ReadLock(synchronizationToken)

  protected val instanceReadWriteLock: WriteLock = new WriteLock(synchronizationToken)

  protected case class Synchronized[T](private var value: T) {

    def apply()(implicit readLock: ReadLock): T = {
      validateLock(readLock, instanceReadLock, instanceReadWriteLock)
      value
    }

    def update[R](f: T => R)(implicit readWriteLock: WriteLock): R = {
      validateLock(readWriteLock, instanceReadWriteLock)
      f(value)
    }

    def swap(newVal: => T)(implicit readWriteLock: WriteLock): T = {
      validateLock(readWriteLock, instanceReadWriteLock)
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
    synchronizeOperation(instanceReadLock)(body)

  protected def write[T](body: WriteLock => T): T =
    synchronizeOperation(instanceReadWriteLock)(body)

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