package com.wavesplatform.serialization.protobuf.utils
import java.io.ByteArrayOutputStream

import com.google.protobuf.{ByteString, CodedOutputStream}
import com.google.protobuf.descriptor.FieldDescriptorProto.Type
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.protobuf.Transaction
import scalapb.GeneratedMessage
import scalapb.descriptors._

object PBUtils extends App {
  def encodeDeterministic(msg: GeneratedMessage): Array[Byte] = {
    val outArray     = new Array[Byte](msg.serializedSize)
    val outputStream = CodedOutputStream.newInstance(outArray)
    outputStream.useDeterministicSerialization() // Adds this
    msg.writeTo(outputStream)
    outputStream.checkNoSpaceLeft()
    outArray
  }

  def encodeForSignature(msg: GeneratedMessage): Array[Byte] = {
    val bs           = new ByteArrayOutputStream(msg.serializedSize * 2)
    val outputStream = CodedOutputStream.newInstance(bs)
    outputStream.useDeterministicSerialization()

    def writeMsgTo(outputStream: CodedOutputStream, msg: GeneratedMessage) = {
      def writeValue(field: FieldDescriptor, value: PValue): Unit = {
        field.protoType match {
          case Type.TYPE_DOUBLE | Type.TYPE_FLOAT | Type.TYPE_GROUP | Type.Unrecognized(_) =>
            throw new IllegalArgumentException(s"Not supported: $field/$value")

          case Type.TYPE_INT64 | Type.TYPE_UINT64 | Type.TYPE_FIXED64 | Type.TYPE_SFIXED64 | Type.TYPE_SINT64 =>
            outputStream.writeFixed64NoTag(value.as[Long])

          case Type.TYPE_FIXED32 | Type.TYPE_INT32 | Type.TYPE_SFIXED32 | Type.TYPE_UINT32 | Type.TYPE_SINT32 =>
            outputStream.writeFixed32NoTag(value.as[Int])

          case Type.TYPE_BOOL =>
            outputStream.writeBoolNoTag(value.as[Boolean])

          case Type.TYPE_STRING =>
            val bytes = value.as[String].toCharArray.map(_.toByte)
            outputStream.writeFixed32NoTag(bytes.length)
            outputStream.write(bytes, 0, bytes.length)

          case Type.TYPE_BYTES =>
            val bytes = value.as[ByteString]
            outputStream.writeFixed32NoTag(bytes.size())
            outputStream.write(bytes.toByteArray, 0, bytes.size())

          case Type.TYPE_ENUM =>
            outputStream.writeFixed32NoTag(value.as[EnumValueDescriptor].index)

          case Type.TYPE_MESSAGE =>
            value.asInstanceOf[PMessage].value.toIndexedSeq.sortBy(_._1.number).foreach(kv => writeValue(kv._1, kv._2))
        }
      }

      msg.companion.scalaDescriptor.fields.foreach { field =>
        if (field.isRepeated) {
          val seq = msg.getField(field).asInstanceOf[PRepeated].value
          seq.foreach(writeValue(field, _))
        } else {
          writeValue(field, msg.getField(field))
        }
      }
    }

    writeMsgTo(outputStream, msg)
    outputStream.flush()
    bs.toByteArray
  }

  val transaction = Transaction(chainId = 123424352)
  val bytes: ByteStr = encodeForSignature(transaction)
  println(bytes)
  println(bytes.hashCode())
}
