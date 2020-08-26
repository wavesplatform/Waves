package com.wavesplatform.api

import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.api.http.ApiError
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Blockchain
import io.grpc.stub.{CallStreamObserver, ServerCallStreamObserver, StreamObserver}
import monix.execution.{Ack, AsyncQueue, Scheduler}
import monix.reactive.Observable

import scala.util.control.NonFatal

package object grpc extends PBImplicitConversions {
  def wrapObservable[A, B](source: Observable[A], dest: StreamObserver[B])(f: A => B)(implicit s: Scheduler): Unit = dest match {
    case cso: CallStreamObserver[B] @unchecked =>
      val queue = AsyncQueue.bounded[B](32)

      cso.setOnReadyHandler { () =>
        pushNext()
      }

      def pushNext(): Unit =
        cso.synchronized {
          while (cso.isReady) queue.tryPoll() match {
            case Some(elem) =>
              cso.onNext(elem)
            case None => return
          }
        }

      val cancelable = source.subscribe(
        (elem: A) => {
          queue
            .offer(f(elem))
            .flatMap { _ =>
              pushNext()
              Ack.Continue
            }
        },
        err => {
          cso.onError(err)
        },
        () => {
          cso.onCompleted()
        }
      )

      cso match {
        case scso: ServerCallStreamObserver[B] @unchecked =>
          scso.setOnCancelHandler(cancelable.cancel _)

        case _ =>
      }

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
    def completeWith(obs: Observable[T]): Unit = {
      wrapObservable(obs, streamObserver)(identity)
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
