package com.wavesplatform.api

import java.util.concurrent.atomic.AtomicReference

import com.wavesplatform.api.http.ApiError
import io.grpc.stub.{CallStreamObserver, ServerCallStreamObserver, StreamObserver}
import io.grpc.{Status, StatusException}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

import scala.concurrent.Future

package object grpc extends PBImplicitConversions {
  implicit class StreamObserverMonixOps[T](streamObserver: StreamObserver[T])(implicit sc: Scheduler) {
    // TODO: More convenient back-pressure implementation
    def toSubscriber: monix.reactive.observers.Subscriber[T] = {
      import org.reactivestreams.{Subscriber, Subscription}

      val rxs = new Subscriber[T] with Cancelable {
        private[this] val element = new AtomicReference[Option[T]](None)

        @volatile
        private[this] var subscription: Subscription = _

        private[this] val observerReadyFunc: () => Boolean = streamObserver match {
          case callStreamObserver: CallStreamObserver[_] =>
            () =>
              callStreamObserver.isReady
          case _ =>
            () =>
              true
        }

        def isReady: Boolean = observerReadyFunc()

        override def onSubscribe(subscription: Subscription): Unit = {
          this.subscription = subscription

          def pushElement(): Unit = element.get() match {
            case v @ Some(value) if this.isReady =>
              if (element.compareAndSet(v, None)) {
                streamObserver.onNext(value)
                subscription.request(1)
              } else {
                pushElement()
              }

            case None if this.isReady =>
              subscription.request(1)

            case _ =>
            // Ignore
          }

          subscription match {
            case scso: ServerCallStreamObserver[T] =>
              scso.disableAutoInboundFlowControl()
              scso.setOnCancelHandler(() => subscription.cancel())
              scso.setOnReadyHandler(() => pushElement())
            // subscription.request(1)

            case cso: CallStreamObserver[T] =>
              cso.disableAutoInboundFlowControl()
              cso.setOnReadyHandler(() => pushElement())
            // subscription.request(1)

            case _ =>
              subscription.request(Long.MaxValue)
          }
        }

        override def onNext(t: T): Unit = {
          if (isReady) {
            val value = element.get()
            if (value.nonEmpty) {
              if (element.compareAndSet(value, Some(t))) streamObserver.onNext(value.get)
              else onNext(t)
            } else {
              streamObserver.onNext(t)
            }
            if (isReady) subscription.request(1)
          } else if (!element.compareAndSet(None, Some(t))) {
            throw new IllegalArgumentException("Buffer overflow")
          }
        }

        override def onError(t: Throwable): Unit = streamObserver.onError(GRPCErrors.toStatusException(t))
        override def onComplete(): Unit          = streamObserver.onCompleted()
        def cancel(): Unit                       = Option(subscription).foreach(_.cancel())
      }

      monix.reactive.observers.Subscriber.fromReactiveSubscriber(rxs, rxs)
    }

    def completeWith(obs: Observable[T]): Cancelable = {
      streamObserver match {
        case _: CallStreamObserver[T] =>
          obs.subscribe(this.toSubscriber)

        case _ => // No back-pressure
          obs
            .doOnError(exception => streamObserver.onError(GRPCErrors.toStatusException(exception)))
            .doOnComplete(() => streamObserver.onCompleted())
            .foreach(value => streamObserver.onNext(value))
      }
    }

    def failWith(error: ApiError): Unit = {
      streamObserver.onError(GRPCErrors.toStatusException(error))
    }
  }

  implicit class OptionToFutureConversionOps[T](opt: Option[T]) {
    def toFuture: Future[T] = opt match {
      case Some(value) => Future.successful(value)
      case None        => Future.failed(new StatusException(Status.NOT_FOUND))
    }

    def toFuture(apiError: ApiError): Future[T] = opt match {
      case Some(value) => Future.successful(value)
      case None        => Future.failed(GRPCErrors.toStatusException(apiError))
    }
  }

  implicit class EitherToFutureConversionOps[E, T](either: Either[E, T])(implicit toThrowable: E => Throwable) {
    def toFuture: Future[T] = {
      val result = either.left
        .map(e => GRPCErrors.toStatusException(toThrowable(e)))
        .toTry

      Future.fromTry(result)
    }
  }

  implicit class ObservableExtensionOps[T](observable: Observable[T]) {
    def optionalLimit(limit: Long): Observable[T] = if (limit > 0) observable.take(limit) else observable
  }
}
