package com.wavesplatform.api
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

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
}
