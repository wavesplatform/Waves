package com.wavesplatform.api.grpc

import com.wavesplatform.api.http.ApiError
import com.wavesplatform.api.http.ApiError._
import io.grpc.Metadata.AsciiMarshaller
import io.grpc.{Metadata, Status, StatusException}

object GRPCErrors {
  private[this] val IntMarshaller: AsciiMarshaller[Int] = new AsciiMarshaller[Int] {
    override def toAsciiString(value: Int): String         = value.toString
    override def parseAsciiString(serialized: String): Int = serialized.toInt
  }

  val ErrorCodeKey = Metadata.Key.of("Error-Code", IntMarshaller)

  def toStatusException(api: ApiError): StatusException = {
    val code = api match {
      case WalletNotExist | WalletAddressDoesNotExist | TransactionDoesNotExist | AliasDoesNotExist(_) | BlockDoesNotExist | MissingSenderPrivateKey |
          DataKeyDoesNotExist =>
        Status.NOT_FOUND
      case WalletAlreadyExists => Status.ALREADY_EXISTS
      case WalletLocked        => Status.PERMISSION_DENIED
      case _                   => Status.INVALID_ARGUMENT
    }

    val metadata = new Metadata()
    metadata.put(ErrorCodeKey, api.id)
    code.withDescription(api.message).asException(metadata)
  }

  def toStatusException(exc: Throwable): StatusException = {
    val status = exc match {
      case _: IllegalArgumentException => Status.INVALID_ARGUMENT
      case _: NoSuchElementException => Status.NOT_FOUND
      case _: IllegalStateException => Status.FAILED_PRECONDITION
      case _ => Status.fromThrowable(exc)
    }
    new StatusException(status.withCause(exc).withDescription(exc.getMessage))
  }
}
