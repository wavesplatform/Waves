package com.wavesplatform.grpc.observers

import com.wavesplatform.grpc.observers.RichGrpcObserver.NoDataTimeoutException
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver, StreamObserver}
import io.grpc.{Status, StatusRuntimeException}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

trait GrpcObserver[EventT] {
  def onReady(): Unit

  /** @return
    *   Ask next?
    */
  def onReceived(event: EventT): Boolean
  def onFailed(error: Throwable): Unit
  def onClosedByRemotePart(): Unit

  def close(): Unit

  def isClosed: Boolean
  def underlying: StreamObserver[EventT]
}

object GrpcObservers {

  final class NoOp[EventT] extends GrpcObserver[EventT] {
    override def onReady(): Unit                    = {}
    override def onReceived(event: EventT): Boolean = false
    override def onFailed(error: Throwable): Unit   = {}
    override def onClosedByRemotePart(): Unit       = {}
    override def close(): Unit                      = {}

    override def isClosed: Boolean = true

    override def underlying: StreamObserver[EventT] = new StreamObserver[EventT] {
      override def onNext(value: EventT): Unit = {}
      override def onError(t: Throwable): Unit = {}
      override def onCompleted(): Unit         = {}
    }

  }

}

abstract class RichGrpcObserver[RequestT, EventT](noDataTimeout: FiniteDuration, hangScheduler: ScheduledExecutorService)
    extends GrpcObserver[EventT] {
  that =>

  private val closed                                            = new AtomicBoolean(false)
  private var requestStream: ClientCallStreamObserver[RequestT] = new NoOpClientCallObserver[RequestT]

  override def close(): Unit =
    if (closed.compareAndSet(false, true)) requestStream.cancel("Closing", new StatusRuntimeException(Status.CANCELLED))

  override def isClosed: Boolean = closed.get()

  override def underlying: StreamObserver[EventT] = new ClientResponseObserver[RequestT, EventT] {

    private val timeout = new AtomicReference[Option[java.util.concurrent.Future[Unit]]](None)

    override def beforeStart(requestStream: ClientCallStreamObserver[RequestT]): Unit = {
      that.requestStream = requestStream
      that.onReady()
    }

    override def onNext(event: EventT): Unit = {
      scheduleHangTimeout()
      if (onReceived(event)) requestStream.request(1) // TODO #34: RichGrpcObserver.onNext: backpressure buffer: Low priority
    }

    override def onError(error: Throwable): Unit = {
      if (!that.isClosed) onFailed(error)
      timeout.get().foreach(_.cancel(false))
    }

    override def onCompleted(): Unit = {
      timeout.get().foreach(_.cancel(false))
      onClosedByRemotePart()
    }

    private def scheduleHangTimeout(): Unit =
      timeout
        .getAndSet(Some(hangScheduler.schedule(() => onFailed(NoDataTimeoutException), noDataTimeout.toMillis, TimeUnit.MILLISECONDS)))
        .foreach(_.cancel(false))

  }

}

object RichGrpcObserver {
  case object NoDataTimeoutException extends TimeoutException("No data") with NoStackTrace
}
