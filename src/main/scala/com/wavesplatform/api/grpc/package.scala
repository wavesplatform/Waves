package com.wavesplatform.api
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future
import scala.util.{Failure, Success}

package object grpc {
  implicit class StreamObserverMonixOps[T](streamObserver: StreamObserver[T])(implicit sc: Scheduler) {
    def completeWith(obs: Observable[T]): Unit = {
      obs
        .foreach(value => streamObserver.onNext(value))
        .onComplete {
          case Success(_)         => streamObserver.onCompleted()
          case Failure(exception) => streamObserver.onError(exception)
        }
    }
  }

  implicit class OptionToFutureConversionOps[T](opt: Option[T]) {
    def toFuture: Future[T] = opt match {
      case Some(value)   => Future.successful(value)
      case None => Future.failed(new NoSuchElementException)
    }
  }
}
