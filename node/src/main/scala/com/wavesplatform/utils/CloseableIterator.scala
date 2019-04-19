package com.wavesplatform.utils
import java.io.Closeable

import scala.collection.{AbstractIterator, GenTraversableOnce, Iterator}

//noinspection ScalaStyle
sealed trait CloseableIterator[+A] extends Iterator[A] with Closeable {
  private[this] def withIterator[B](iterator: Iterator[B]): CloseableIterator[B] =
    CloseableIterator(
      iterator,
      () => this.close()
    )

  def transform[B](f: Iterator[A] => Iterator[B]): CloseableIterator[B]    = withIterator(f(this))
  override def collect[B](pf: PartialFunction[A, B]): CloseableIterator[B] = withIterator(super.collect(pf))
  override def map[B](f: A => B): CloseableIterator[B]                     = withIterator(super.map(f))
  override def filter(p: A => Boolean): CloseableIterator[A]               = withIterator(super.filter(p))
  override def filterNot(p: A => Boolean): CloseableIterator[A]            = withIterator(super.filterNot(p))
  override def take(n: Int): CloseableIterator[A]                          = withIterator(super.take(n))
  override def drop(n: Int): CloseableIterator[A]                          = withIterator(super.drop(n))
  override def takeWhile(p: A => Boolean): CloseableIterator[A]            = withIterator(super.takeWhile(p))
  override def dropWhile(p: A => Boolean): CloseableIterator[A]            = withIterator(super.dropWhile(p))

  override def flatMap[B](f: A => GenTraversableOnce[B]): CloseableIterator[B] = {
    @volatile
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
        } finally this.close()
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
    new DeferredCloseableIterator(() => f)
  }

  def close(iterator: Iterator[_]): Unit = iterator match {
    case c: Closeable => c.close()
    case _            => // Ignore
  }

  def using[T, V](iterator: CloseableIterator[T])(f: CloseableIterator[T] => V): V =
    iterator.closeAfter(f)

  implicit def fromIterator[T](iterator: Iterator[T]): CloseableIterator[T] = iterator match {
    case c: CloseableIterator[T] => c
    case _                       => apply(iterator, () => ())
  }

  private[this] class DeferredCloseableIterator[+T](createIterator: () => CloseableIterator[T]) extends CloseableIterator[T] {
    //noinspection ScalaStyle
    private[this] var lazyIterator: CloseableIterator[T] = _

    private[this] def underlying: CloseableIterator[T] = {
      if (lazyIterator == null) synchronized(if (lazyIterator == null) lazyIterator = createIterator().ensuring(_ != null, "Created iterator shouldn't be null"))
      lazyIterator
    }

    override def close(): Unit    = synchronized(if (lazyIterator != null) lazyIterator.close())
    override def hasNext: Boolean = underlying.hasNext
    override def next(): T        = underlying.next()
  }
}
