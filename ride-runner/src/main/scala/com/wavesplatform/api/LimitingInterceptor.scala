package com.wavesplatform.api

import io.grpc.*

import java.util.concurrent.Semaphore

class LimitingInterceptor(maxConcurrentCalls: Int) extends ClientInterceptor {
  private val semaphore = new Semaphore(maxConcurrentCalls)

  override def interceptCall[ReqT, RespT](
      methodDescriptor: MethodDescriptor[ReqT, RespT],
      callOptions: CallOptions,
      channel: Channel
  ): ClientCall[ReqT, RespT] = {
    if (semaphore.availablePermits() == 0) GrpcStats.blockedCall(GrpcMethod.of(methodDescriptor)).increment()
    semaphore.acquire()
    new ForwardingClientCall.SimpleForwardingClientCall[ReqT, RespT](channel.newCall(methodDescriptor, callOptions)) {
      override def start(responseListener: ClientCall.Listener[RespT], headers: Metadata): Unit = super.start(
        new ForwardingClientCallListener.SimpleForwardingClientCallListener[RespT](responseListener) {
          override def onClose(status: Status, trailers: Metadata): Unit = {
            semaphore.release()
            super.onClose(status, trailers)
          }
        },
        headers
      )
    }
  }
}
