package com.wavesplatform.grpc.observers

import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import monix.execution.exceptions.UpstreamTimeoutException

object ClosedByRemotePart {
  def unapply(arg: WrappedEvent[SubscribeEvent]): Boolean =
    arg match {
      case WrappedEvent.Closed | WrappedEvent.Failed(_: UpstreamTimeoutException) => true
      case _                                                                      => false
    }
}
