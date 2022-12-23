package com.wavesplatform.events

sealed trait WrappedEvent[+T] extends Product with Serializable
object WrappedEvent {
  case class Next[T](payload: T)      extends WrappedEvent[T]
  case class Failed(error: Throwable) extends WrappedEvent[Nothing]
  case object Closed                  extends WrappedEvent[Nothing]
}
