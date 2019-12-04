package com.wavesplatform.transaction

import scala.util.control.NoStackTrace

sealed trait TxParseError extends NoStackTrace

case class PBParsingError(underlying: Throwable) extends TxParseError {
  override val getMessage: String = s"Error while parsing protobuf transaction: ${underlying.getMessage}"
}

case class UnexpectedTransaction(expected: Byte, actual: Byte) extends TxParseError {
  override val getMessage: String = s"Unexpected transaction - required: $expected, actual: $actual"
}

case class UnknownType(typeId: Byte) extends TxParseError {
  override val getMessage: String = s"Unknown transaction type (old encoding): '$typeId'"
}

case class UnknownTypeAndVersion(typeId: Byte, version: Byte) extends TxParseError {
  override val getMessage: String = s"Unknown transaction type ($typeId) and version ($version) (modern encoding)"
}

case object BufferUnderflow extends TxParseError {
  override val getMessage: String = "Buffer underflow while parsing transaction"
}
