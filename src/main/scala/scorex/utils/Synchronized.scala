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

  protected def instanceReadLock: ReadLock = new ReadLock(synchronizationToken)

  protected def instanceReadWriteLock: WriteLock = new WriteLock(synchronizationToken)

  protected case class Synchronized[T](private var value: T) {

    def apply()(implicit readLock: ReadLock): T = {
      value
    }

    def mutate[R](f: T => R)(implicit readWriteLock: WriteLock): R = {
      f(value)
    }

    def set(newVal: => T)(implicit readWriteLock: WriteLock): T = {
      val oldVal = value
      value = newVal
      oldVal
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