package com.wavesplatform.utils
import java.io.Closeable

import scala.collection.AbstractIterator

sealed trait CloseableIterator[+T] extends Iterator[T] with Closeable {
  def transform[NewT](f: Iterator[T] => Iterator[NewT]): CloseableIterator[NewT] = {
    CloseableIterator(
      f(this),
      () => this.close()
    )
  }
}

object CloseableIterator {
  val empty: CloseableIterator[Nothing] = apply(Iterator.empty, () => ())

  def apply[T](iterator: Iterator[T], doClose: () => Unit): CloseableIterator[T] = new AbstractIterator[T] with CloseableIterator[T] {
    private[this] var closed = false

    override def close(): Unit = {
      if (!closed) synchronized(if (!closed) {
        doClose()
        closed = true
      })
    }

    override def hasNext: Boolean = !closed && {
      val hasNext = iterator.hasNext
      if (!hasNext) close()
      hasNext
    }

    override def next(): T = iterator.next()

    override def finalize(): Unit = {
      if (!this.closed) {
        System.err.println(s"CloseableIterator leaked: $this [${System.identityHashCode(this)}]")
        this.close()
      }
      super.finalize()
    }
  }

  def seq[T](iterators: CloseableIterator[T]*): CloseableIterator[T] = {
    apply(
      iterators.fold(Iterator.empty: Iterator[T])(_ ++ _),
      () => iterators.foreach(_.close())
    )
  }

  def close(iterator: Iterator[_]): Unit = iterator match {
    case c: Closeable => c.close()
    case _ => // Ignore
  }

  implicit def fromIterator[T](iterator: Iterator[T]): CloseableIterator[T] = apply(iterator, () => ())
}
