package com.wavesplatform.events.api.grpc
import java.util.concurrent.LinkedBlockingQueue

import com.wavesplatform.utils.ScorexLogging
import io.grpc.{Status, StatusRuntimeException}
import io.grpc.stub.{CallStreamObserver, ServerCallStreamObserver, StreamObserver}
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable

object backpressure {
  implicit class StreamObserverMonixOps[T](streamObserver: StreamObserver[T])(implicit sc: Scheduler) extends ScorexLogging {
    // TODO: More convenient back-pressure implementation
    def toSubscriber: monix.reactive.observers.Subscriber[T] = {
      import org.reactivestreams.{Subscriber, Subscription}

      val rxs = new Subscriber[T] with Cancelable {
        private[this] val queue = new LinkedBlockingQueue[T](32)

        @volatile
        private[this] var subscription: Subscription = _

        private[this] val observerReadyFunc: () => Boolean = streamObserver match {
          case callStreamObserver: CallStreamObserver[_] =>
            () => callStreamObserver.isReady
          case _ =>
            () => true
        }

        def isReady: Boolean = observerReadyFunc()

        override def onSubscribe(subscription: Subscription): Unit = {
          this.subscription = subscription

          def pushElement(): Unit = Option(queue.peek()) match {
            case Some(_) if this.isReady =>
              val qv = queue.poll()
              streamObserver.onNext(qv)
              subscription.request(1)

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

            case cso: CallStreamObserver[T] =>
              cso.disableAutoInboundFlowControl()
              cso.setOnReadyHandler(() => pushElement())

            case _ =>
              subscription.request(Long.MaxValue)
          }
        }

        override def onNext(t: T): Unit = {
          queue.add(t)
          if (isReady) {
            val value = Option(queue.poll())
            value.foreach(streamObserver.onNext)
            if (isReady) subscription.request(1)
          }
        }

        override def onError(t: Throwable): Unit = {
          log.error("gRPC streaming error", t)
          streamObserver.onError(toStatusException(t))
        }
        override def onComplete(): Unit          = streamObserver.onCompleted()
        def cancel(): Unit                       = Option(subscription).foreach(_.cancel())
      }

      monix.reactive.observers.Subscriber.fromReactiveSubscriber(rxs, rxs)
    }

    def completeWith(obs: Observable[T]): Cancelable = {
      streamObserver match {
        case _: CallStreamObserver[T] =>
          log.info("Subscribed with backpressure")
          obs.subscribe(this.toSubscriber)

        case _ => // No back-pressure
          log.warn("Subscribed without backpressure")
          obs
            .doOnError(exception => Task(streamObserver.onError(toStatusException(exception))))
            .doOnComplete(Task(streamObserver.onCompleted()))
            .foreach(value => streamObserver.onNext(value))
      }
    }

    def toStatusException(t: Throwable) =
      new StatusRuntimeException(Status.INTERNAL.withDescription(t.getMessage))
  }
}
