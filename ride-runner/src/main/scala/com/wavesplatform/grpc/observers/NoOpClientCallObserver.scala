package com.wavesplatform.grpc.observers

import io.grpc.stub.ClientCallStreamObserver

final class NoOpClientCallObserver[RequestT] extends ClientCallStreamObserver[RequestT] {
  override def cancel(message: String, cause: Throwable): Unit   = {}
  override def isReady: Boolean                                  = false
  override def setOnReadyHandler(onReadyHandler: Runnable): Unit = {}
  override def request(count: Int): Unit                         = {}
  override def setMessageCompression(enable: Boolean): Unit      = {}
  override def disableAutoInboundFlowControl(): Unit             = {}
  override def onNext(value: RequestT): Unit                     = {}
  override def onError(t: Throwable): Unit                       = {}
  override def onCompleted(): Unit                               = {}
}
