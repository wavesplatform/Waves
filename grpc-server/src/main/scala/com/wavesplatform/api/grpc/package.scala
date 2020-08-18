package com.wavesplatform.api

import java.util.concurrent.LinkedBlockingQueue

import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Blockchain
import io.grpc.stub.{CallStreamObserver, StreamObserver}
import monix.eval.Task
import monix.execution.{Ack, Cancelable, Scheduler}
import monix.reactive.Observable

import scala.concurrent.Future
import scala.util.control.NonFatal

package object grpc extends PBImplicitConversions {
  def wrapObservable[A, B](source: Observable[A], dest: StreamObserver[B])(f: A => B)(implicit s: Scheduler): Cancelable = dest match {
    case cso: CallStreamObserver[B] @unchecked =>
      val queue = new LinkedBlockingQueue[B](32)

      def pushNext(): Unit =
        cso.synchronized(while (cso.isReady && !queue.isEmpty) {
          cso.onNext(queue.poll())
        })

      cso.setOnReadyHandler(pushNext _)

      source.subscribe(
        (elem: A) =>
          if (queue.offer(f(elem))) {
            pushNext()
            Ack.Continue
          } else
            Future {
              queue.put(f(elem))
              pushNext()
            }.flatMap(_ => Ack.Continue),
        cso.onError,
        cso.onCompleted
      )
    case _ =>
      source.subscribe(
        { (elem: A) =>
          dest.onNext(f(elem)); Ack.Continue
        },
        dest.onError,
        dest.onCompleted
      )
  }

  implicit class StreamObserverMonixOps[T](streamObserver: StreamObserver[T])(implicit sc: Scheduler) {
    def completeWith(obs: Observable[T]): Cancelable = {
      streamObserver match {
        case _: CallStreamObserver[T] =>
          wrapObservable(obs, streamObserver)(identity)

        case _ => // No back-pressure
          obs
            .doOnError(exception => Task(streamObserver.onError(GRPCErrors.toStatusException(exception))))
            .doOnComplete(Task(streamObserver.onCompleted()))
            .foreach(value => streamObserver.onNext(value))
      }
    }

    def failWith(error: ApiError): Unit = {
      streamObserver.onError(GRPCErrors.toStatusException(error))
    }

    def interceptErrors(f: => Unit): Unit =
      try f
      catch { case NonFatal(e) => streamObserver.onError(GRPCErrors.toStatusException(e)) }
  }

  implicit class EitherVEExt[T](e: Either[ValidationError, T]) {
    def explicitGetErr(): T = e.fold(e => throw GRPCErrors.toStatusException(e), identity)
  }

  implicit class OptionErrExt[T](e: Option[T]) {
    def explicitGetErr(err: ApiError): T = e.getOrElse(throw GRPCErrors.toStatusException(err))
  }

  implicit class AddressOrAliasExt(a: AddressOrAlias) {
    def resolved(blockchain: Blockchain): Option[Address] = blockchain.resolveAlias(a).toOption
  }
}
