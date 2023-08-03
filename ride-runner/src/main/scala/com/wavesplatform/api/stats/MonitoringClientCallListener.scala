package com.wavesplatform.api.stats

import com.wavesplatform.api.{GrpcMethod, GrpcStats}
import io.grpc.{ClientCall, ForwardingClientCallListener, Metadata, Status}

class MonitoringClientCallListener[RespT](
    override val delegate: ClientCall.Listener[RespT],
    method: GrpcMethod
) extends ForwardingClientCallListener[RespT] {
  private val start = System.nanoTime()

  override def onClose(status: Status, metadata: Metadata): Unit = {
    GrpcStats.status(method, status.getCode).increment()
    GrpcStats.latency(method).record(System.nanoTime() - start)
    super.onClose(status, metadata)
  }
}
