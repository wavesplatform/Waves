package com.wavesplatform.transaction.protobuf
import com.google.protobuf.ByteString

sealed trait ChainId {
  def byte: Byte
  def isEmpty: Boolean = byte == 0
}

object ChainId {
  def empty = NoChainId

  implicit def fromByteString(bytes: ByteString): ChainId =
    if (bytes.isEmpty) NoChainId
    else
      bytes.byteAt(0) match {
        case 0 => NoChainId
        case b => ChainIdByte(b)
      }

  implicit def toByteString(chainId: ChainId): ByteString = {
    if (chainId.isEmpty) ByteString.EMPTY else ByteString.copyFrom(Array(chainId.byte))
  }

  implicit def fromByte(byte: Byte): ChainId = byte match {
    case 0 => NoChainId
    case _ => ChainIdByte(byte)
  }

  implicit def fromByteOption(byteOption: Option[Byte]): ChainId = byteOption match {
    case None | Some(0) => NoChainId
    case Some(byte)     => ChainIdByte(byte)
  }

  implicit def toByte(chainId: ChainId): Byte = chainId.byte

  final case class ChainIdByte(byte: Byte) extends ChainId
  case object NoChainId extends ChainId {
    def byte: Byte = 0
  }
}
