package com.wavesplatform.api.grpc.test

import com.wavesplatform.utils.Schedulers
import io.grpc.stub.StreamObserver
import monix.eval.Task
import monix.execution.ExecutionModel.SynchronousExecution
import monix.execution.Scheduler
import monix.reactive.subjects.ConcurrentSubject

trait GrpcApiHelpers {
  private given scheduler: Scheduler = Schedulers.singleThread("grpc", executionModel = SynchronousExecution)
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
