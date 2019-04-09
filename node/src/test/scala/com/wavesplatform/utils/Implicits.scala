package com.wavesplatform.utils

import monix.execution.{Ack, Cancelable}
import monix.reactive.observers.Subscriber
import monix.reactive.subjects.Subject

import scala.concurrent.Future

object Implicits {
  final implicit class SubjectOps(val self: Subject.type) extends AnyVal {
    def empty[T]: Subject[T, T] = new Subject[T, T] {
      override def size: Int                                                = 0
      override def unsafeSubscribeFn(subscriber: Subscriber[T]): Cancelable = Cancelable.empty
      override def onNext(elem: T): Future[Ack]                             = Future.successful(Ack.Stop)
      override def onError(ex: Throwable): Unit                             = {}
      override def onComplete(): Unit                                       = {}
    }
  }
}
