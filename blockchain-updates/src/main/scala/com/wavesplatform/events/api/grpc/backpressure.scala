package com.wavesplatform.events.api.grpc
import java.util.concurrent.LinkedBlockingQueue

import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.{CallStreamObserver, ServerCallStreamObserver, StreamObserver}
import io.grpc.{Status, StatusRuntimeException}
import monix.eval.Task
import monix.execution.{Ack, AsyncQueue, Cancelable, Scheduler}
import monix.reactive.Observable

//noinspection ScalaStyle
object backpressure extends ScorexLogging {
  def wrapObservable[A, B](source: Observable[A], dest: StreamObserver[B])(f: A => B)(implicit s: Scheduler): Unit = dest match {
    case cso: CallStreamObserver[B] @unchecked =>
      val csoHash = Integer.toHexString(System.identityHashCode(dest))
      log.info(s"[$csoHash] Connecting stream observer")
      val queue   = AsyncQueue.bounded[B](32)

      cso.setOnReadyHandler { () =>
       log.info(s"[$csoHash] Stream ready")
        pushNext()
      }

      def pushNext(): Unit =
        cso.synchronized {
          if (!cso.isReady) return
          log.info(s"[$csoHash] Starting poll")
          var exit = false
          while (cso.isReady && !exit) queue.tryPoll() match {
            case Some(elem) =>
              log.info(s"[$csoHash] Sending element: ${elem.getClass.getSimpleName}#${Integer.toHexString(System.identityHashCode(elem))}")
              cso.onNext(elem)
            case None       => exit = true
          }
          log.info(s"[$csoHash] Ending poll, cso ready = ${cso.isReady}")
        }

      val cancelable = source.subscribe(
        (elem: A) => {
          log.info(s"[$csoHash] Offering new element: ${elem.getClass.getSimpleName}#${Integer.toHexString(System.identityHashCode(elem))}")
          queue
            .offer(f(elem))
            .flatMap { _ =>
              //log.info(s"[$csoHash] Element added, starting poll: $elem")
              pushNext()
              Ack.Continue
            }
        },
        err => {
          log.error(s"[$csoHash] Stream error", err)
          cso.onError(err)
        },
        () => {
          log.info(s"[$csoHash] Stream completed")
          cso.onCompleted()
        }
      )

      cso match {
        case scso: ServerCallStreamObserver[B] @unchecked =>
          scso.setOnCancelHandler(cancelable.cancel _)

        case _ =>
          log.warn("Couldn't bind onCancel handler")
      }

    case _ =>
      log.warn(s"Connecting without back-pressure: $dest")
      source.subscribe(
        { (elem: A) =>
          dest.onNext(f(elem)); Ack.Continue
        },
        dest.onError,
        () => dest.onCompleted()
      )

  }

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
        override def onComplete(): Unit = streamObserver.onCompleted()
        def cancel(): Unit              = Option(subscription).foreach(_.cancel())
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
