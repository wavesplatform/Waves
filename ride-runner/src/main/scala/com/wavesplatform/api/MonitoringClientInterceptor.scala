package com.wavesplatform.api

import io.grpc.*

object MonitoringClientInterceptor extends ClientInterceptor {
  override def interceptCall[ReqT, RespT](
      methodDescriptor: MethodDescriptor[ReqT, RespT],
      callOptions: CallOptions,
      channel: Channel
  ): ClientCall[ReqT, RespT] = new ForwardingClientCall.SimpleForwardingClientCall[ReqT, RespT](channel.newCall(methodDescriptor, callOptions)) {
    override def start(responseListener: ClientCall.Listener[RespT], headers: Metadata): Unit = super.start(
      new ForwardingClientCallListener.SimpleForwardingClientCallListener[RespT](responseListener) {
        private val start = System.nanoTime()
        override def onClose(status: Status, trailers: Metadata): Unit = {
          val method = GrpcMethod.of(methodDescriptor)
          GrpcStats.status(method, status.getCode).increment()
          GrpcStats.latency(method).record(System.nanoTime() - start)
          super.onClose(status, trailers)
        }
      },
      headers
    )
  }
}
