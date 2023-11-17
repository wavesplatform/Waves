package com.wavesplatform.api

import io.grpc.{CallOptions, Channel, ClientCall, MethodDescriptor}

object EmptyChannel extends Channel {
  override def authority(): String = ???
  override def newCall[RequestT, ResponseT](
      methodDescriptor: MethodDescriptor[RequestT, ResponseT],
      callOptions: CallOptions
  ): ClientCall[RequestT, ResponseT] = ???
}
