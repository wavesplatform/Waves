package com.wavesplatform.utils
import java.io.Closeable
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.{AbstractIterator, GenTraversableOnce, Iterator}

//noinspection ScalaStyle
sealed trait CloseableIterator[+A] extends Iterator[A] with Closeable {
  def transform[B](f: Iterator[A] => Iterator[B]): CloseableIterator[B] = {
    CloseableIterator(
      f(this),
      () => this.close()
    )
  }

  override def collect[B](pf: PartialFunction[A, B]): CloseableIterator[B] = transform(_ => super.collect(pf))
  override def map[B](f: A => B): CloseableIterator[B]                     = transform(_ => super.map(f))
  override def filter(p: A => Boolean): CloseableIterator[A]               = transform(_ => super.filter(p))
  override def take(n: Int): CloseableIterator[A]                          = transform(_ => super.take(n))
  override def drop(n: Int): CloseableIterator[A]                          = transform(_ => super.drop(n))
  override def takeWhile(p: A => Boolean): CloseableIterator[A]            = transform(_ => super.takeWhile(p))
  override def dropWhile(p: A => Boolean): CloseableIterator[A]            = transform(_ => super.dropWhile(p))

  override def flatMap[B](f: A => GenTraversableOnce[B]): CloseableIterator[B] = {
    @transient
    var closeables = List.empty[Closeable]

    CloseableIterator(
      super.flatMap { value =>
        val result = f(value)
        result match {
          case c: Closeable =>
            closeables = c :: closeables
            result

          case _ =>
            result
        }
      }, { () =>
        try {
          closeables.foreach(_.close())
        } finally {
          this.close()
        }
      }
    )
  }

  def closeAfter[V](f: this.type => V): V = {
    try {
      f(this)
    } finally this.close()
  }

  def ++[NewT >: A](c: => CloseableIterator[NewT]): CloseableIterator[NewT] = {
    lazy val right = c

    CloseableIterator(
      super.++(right), { () =>
        this.close()
        right.close()
      }
    )
  }
}

object CloseableIterator {
  def empty[T]: CloseableIterator[T] = apply(Iterator.empty, () => ())

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

  def defer[T](f: => CloseableIterator[T]): CloseableIterator[T] = {
    val initialized = new AtomicBoolean(false)

    //noinspection ScalaStyle
    @volatile var lazyIterator: CloseableIterator[T] = null

    def createIteratorInstance() = {
      if (initialized.compareAndSet(false, true)) lazyIterator = f
      lazyIterator
    }

    CloseableIterator(
      new AbstractIterator[T] {
        private[this] var iteratorInstance: Iterator[T] = _
        override def hasNext: Boolean = {
          if (iteratorInstance == null) iteratorInstance = createIteratorInstance()
          iteratorInstance.hasNext
        }
        override def next(): T = {
          if (iteratorInstance == null) iteratorInstance = createIteratorInstance()
          iteratorInstance.next()
        }
      },
      () => if (lazyIterator != null) lazyIterator.close()
    )
  }

  def close(iterator: Iterator[_]): Unit = iterator match {
    case c: Closeable => c.close()
    case _            => // Ignore
  }

  implicit def fromIterator[T](iterator: Iterator[T]): CloseableIterator[T] = apply(iterator, () => ())
}
