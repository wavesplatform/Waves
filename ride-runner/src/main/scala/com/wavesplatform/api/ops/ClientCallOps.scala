package com.wavesplatform.api.ops

import io.grpc.{ClientCall, Grpc}

import java.net.SocketAddress

trait ClientCallSyntax {

  @`inline` implicit final def clientCallSyntaxOps[RequestT, ResponseT](self: ClientCall[RequestT, ResponseT]): ClientCallOps[RequestT, ResponseT] =
    new ClientCallOps(self)

}

final class ClientCallOps[RequestT, ResponseT](val self: ClientCall[RequestT, ResponseT]) extends AnyVal {
  def socketAddress: Option[SocketAddress] = Option(self.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR))
  def socketAddressStr: String             = socketAddress.fold("unknown")(_.toString)
}
