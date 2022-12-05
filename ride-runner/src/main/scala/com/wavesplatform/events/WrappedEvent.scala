package com.wavesplatform.events

sealed trait WrappedEvent[+T] extends Product with Serializable
object WrappedEvent {
  case class Next[T](payload: T)      extends WrappedEvent[T]
  case class Failed(error: Throwable) extends WrappedEvent[Nothing]
  case object Closed                  extends WrappedEvent[Nothing]

  implicit final class Ops[A](val self: WrappedEvent[A]) extends AnyVal {
    def map[B](f: A => B): WrappedEvent[B] = self match {
      case Next(payload) => Next(f(payload))
      case Failed(error) => Failed(error)
      case Closed        => Closed
    }
  }
}
