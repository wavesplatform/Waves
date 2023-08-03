package com.wavesplatform.api.stats

import com.wavesplatform.api.GrpcMethod
import io.grpc.{ClientCall, ForwardingClientCall, Metadata}

class MonitoringClientCall[R, S](
    delegate: ClientCall[R, S],
    grpcMethod: GrpcMethod
) extends ForwardingClientCall.SimpleForwardingClientCall[R, S](delegate) {
  override def start(delegate: ClientCall.Listener[S], metadata: Metadata): Unit =
    super.start(new MonitoringClientCallListener[S](delegate, grpcMethod), metadata)
}
