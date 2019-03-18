package com.wavesplatform.protobuf
import com.wavesplatform.protobuf.utils.PBUtils
import monix.eval.Coeval
import scalapb.GeneratedMessage

private[protobuf] sealed trait PBSerializable {
  def serializedSize: Int
  def toBytes: Array[Byte]
}

private[protobuf] object PBSerializable {
  implicit class PBRawBytesSerializable(private val underlyingBytes: Array[Byte]) extends PBSerializable {
    override def serializedSize: Int =
      _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(underlyingBytes.length) + underlyingBytes.length
    override def toBytes: Array[Byte] = underlyingBytes
  }

  implicit class PBMessageSerializable(val underlyingMessage: GeneratedMessage) extends PBSerializable {
    private[this] val bytesCoeval = Coeval.evalOnce(PBUtils.encodeDeterministic(underlyingMessage))
    override def serializedSize: Int =
      _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(underlyingMessage.serializedSize) + underlyingMessage.serializedSize
    override def toBytes: Array[Byte] = bytesCoeval()
  }
}
