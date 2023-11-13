package monix.reactive.operators

import monix.execution.Ack.Continue
import monix.execution.cancelables.OrderedCancelable
import monix.execution.{Ack, Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.observers.Subscriber

import scala.concurrent.Future
import scala.util.Success
import scala.util.control.NonFatal

private final class OnErrorRetryWithObservable[A](source: Observable[A], p: PartialFunction[Throwable, A]) extends Observable[A] {

  private def loop(subscriber: Subscriber[A], task: OrderedCancelable, retryIdx: Long, firstMessage: Option[A]): Unit = {
    val cancelable = source.unsafeSubscribeFn(new Subscriber[A] {
      implicit val scheduler: Scheduler = subscriber.scheduler
      private[this] var isDone          = false

      // This should be here, calling subscriber.onNext in onError doesn't work
      private[this] var ack: Future[Ack] = firstMessage.fold[Future[Ack]](Continue)(subscriber.onNext)

      def onNext(elem: A): Future[Ack] = {
        ack = subscriber.onNext(elem)
        ack
      }

      def onComplete(): Unit =
        if (!isDone) {
          isDone = true
          subscriber.onComplete()
        }

      def onError(ex: Throwable): Unit =
        if (!isDone) {
          isDone = true

          // Protects calls to user code from within the operator and
          // stream the error downstream if it happens, but if the
          // error happens because of calls to `onNext` or other
          // protocol calls, then the behavior should be undefined.
          var streamError = true
          try {
            val firstMessageAfterRecover = p.lift(ex)
            streamError = false

            // NOTE: The code is the same as in OnErrorRetryIfObservable, except this place
            firstMessageAfterRecover match {
              case Some(_) =>
                // need asynchronous execution to avoid a synchronous loop
                // blowing out the call stack
                ack.onComplete {
                  case Success(Continue) =>
                    loop(subscriber, task, retryIdx + 1, firstMessageAfterRecover)
                  case _ =>
                    () // stop
                }

              case None => subscriber.onError(ex)
            }
          } catch {
            case NonFatal(err) if streamError =>
              scheduler.reportFailure(ex)
              subscriber.onError(err)
          }
        }
    })

    // We need to do an `orderedUpdate`, because `onError` might have
    // already executed and we might be resubscribed by now.
    task.orderedUpdate(cancelable, retryIdx)
    ()
  }

  def unsafeSubscribeFn(subscriber: Subscriber[A]): Cancelable = {
    val task = OrderedCancelable()
    loop(subscriber, task, retryIdx = 0, firstMessage = None)
    task
  }
}
