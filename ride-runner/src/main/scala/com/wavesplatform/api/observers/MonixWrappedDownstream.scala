package com.wavesplatform.api.observers

import com.wavesplatform.events.WrappedEvent
import monix.reactive.Observer

class MonixWrappedDownstream[RequestT, EventT](s: Observer[WrappedEvent[EventT]]) extends ManualGrpcObserver[RequestT, EventT] {
  override def onNext(value: EventT): Unit = send(WrappedEvent.Next(value))

  override def onError(t: Throwable): Unit = {
    super.onError(t)
    send(WrappedEvent.Failed(t))
  }

  override def onCompleted(): Unit = {
    super.onCompleted()
    send(WrappedEvent.Closed)
  }

  private def send(event: WrappedEvent[EventT]): Unit = ifWorking(s.onNext(event))
}
