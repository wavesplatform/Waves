package com.wavesplatform.api.grpc.test

import io.grpc.stub.StreamObserver
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.ConcurrentSubject

trait GrpcApiHelpers {
  def createObserver[T]: (StreamObserver[T], Task[List[T]]) = {
    val subj = ConcurrentSubject.replay[T]

    val observer = new StreamObserver[T] {
      override def onNext(value: T): Unit      = subj.onNext(value)
      override def onError(t: Throwable): Unit = subj.onError(t)
      override def onCompleted(): Unit         = subj.onComplete()
    }

    (observer, subj.toListL)
  }
}
