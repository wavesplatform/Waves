package com.wavesplatform.matcher

import cats.Functor
import cats.mtl.FunctorEmpty

trait ClosableIterable[+T] extends AutoCloseable {
  val iterator: Iterator[T]
}

object ClosableIterable {
  def from[A, B](xs: ClosableIterable[A])(f: ClosableIterable[A] => Iterator[B]): ClosableIterable[B] = new ClosableIterable[B] {
    override val iterator: Iterator[B] = f(xs)
    override def close(): Unit         = xs.close()
  }

  val empty: ClosableIterable[Nothing] = new ClosableIterable[Nothing] {
    override val iterator: Iterator[Nothing] = Iterator.empty
    override def close(): Unit               = {}
  }

  implicit val functor: Functor[ClosableIterable] = new Functor[ClosableIterable] {
    override def map[A, B](fa: ClosableIterable[A])(f: A => B): ClosableIterable[B] = from(fa)(_.iterator.map(f))
  }

  implicit val functorEmpty: FunctorEmpty[ClosableIterable] = new FunctorEmpty[ClosableIterable] {
    override val functor: Functor[ClosableIterable]                                                    = implicitly
    override def mapFilter[A, B](fa: ClosableIterable[A])(f: A => Option[B]): ClosableIterable[B]      = flattenOption(functor.map(fa)(f))
    override def collect[A, B](fa: ClosableIterable[A])(f: PartialFunction[A, B]): ClosableIterable[B] = from(fa)(_.iterator.collect(f))
    override def flattenOption[A](fa: ClosableIterable[Option[A]]): ClosableIterable[A]                = collect(fa) { case Some(x) => x }
    override def filter[A](fa: ClosableIterable[A])(f: A => Boolean): ClosableIterable[A]              = from(fa)(_.iterator.filter(f))
  }
}
