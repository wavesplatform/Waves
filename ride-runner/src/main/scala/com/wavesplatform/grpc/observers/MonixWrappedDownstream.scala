package com.wavesplatform.grpc.observers

import com.wavesplatform.events.WrappedEvent
import monix.reactive.Observer

class MonixWrappedDownstream[RequestT, EventT](s: Observer[WrappedEvent[EventT]]) extends ManualGrpcObserver[RequestT, EventT] {
  override def close(): Unit = {
    log.info("Closing")
    super.close()
  }

  override def onNext(value: EventT): Unit            = send(WrappedEvent.Next(value))
  override def onError(t: Throwable): Unit            = send(WrappedEvent.Failed(t))
  override def onCompleted(): Unit                    = send(WrappedEvent.Closed)
  private def send(event: WrappedEvent[EventT]): Unit = ifWorking(s.onNext(event))
}
