package com.wavesplatform.api.stats

import com.wavesplatform.api.GrpcMethod
import io.grpc.*

object MonitoringClientInterceptor extends ClientInterceptor {
  override def interceptCall[R, S](methodDescriptor: MethodDescriptor[R, S], callOptions: CallOptions, channel: Channel): ClientCall[R, S] =
    new MonitoringClientCall[R, S](channel.newCall(methodDescriptor, callOptions), GrpcMethod.of(methodDescriptor))
}
