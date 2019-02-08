package com.wavesplatform.serialization.protobuf.json

import _root_.scalapb.descriptors._
import com.google.protobuf.ByteString
import com.google.protobuf.descriptor.FieldDescriptorProto
import com.google.protobuf.duration.Duration
import com.google.protobuf.struct.NullValue
import com.google.protobuf.timestamp.Timestamp
import com.wavesplatform.common.utils.Base58
import org.json4s.JsonAST._
import org.json4s.{Reader, Writer}
import scalapb._
import scalapb.json4s._

import scala.language.existentials

private[wavesplatform] object PBWavesJsonFormat {
  class FixedPrinter(includingDefaultValueFields: Boolean = false,
                     preservingProtoFieldNames: Boolean = false,
                     formattingLongAsNumber: Boolean = true,
                     formattingEnumsAsNumber: Boolean = false,
                     formatRegistry: FormatRegistry = JsonFormat.DefaultRegistry,
                     typeRegistry: TypeRegistry = TypeRegistry.empty)
      extends Printer(includingDefaultValueFields,
                      preservingProtoFieldNames,
                      formattingLongAsNumber,
                      formattingEnumsAsNumber,
                      formatRegistry,
                      typeRegistry) {

    override def serializeSingleValue(fd: FieldDescriptor, value: PValue, formattingLongAsNumber: Boolean): JValue = value match {
      case PByteString(v) => JString(Base58.encode(v.toByteArray))
      case _              => super.serializeSingleValue(fd, value, formattingLongAsNumber)
    }
  }

  import com.google.protobuf.wrappers

  type GenericCompanion = GeneratedMessageCompanion[T] forSome { type T <: GeneratedMessage with Message[T] }

  val DefaultRegistry = FormatRegistry()
    .registerWriter(
      (d: Duration) => JString(Durations.writeDuration(d)),
      jv =>
        jv match {
          case JString(str) => Durations.parseDuration(str)
          case _            => throw new JsonFormatException("Expected a string.")
      }
    )
    .registerWriter(
      (t: Timestamp) => JString(Timestamps.writeTimestamp(t)),
      jv =>
        jv match {
          case JString(str) => Timestamps.parseTimestamp(str)
          case _            => throw new JsonFormatException("Expected a string.")
      }
    )
    .registerMessageFormatter[wrappers.DoubleValue](primitiveWrapperWriter, primitiveWrapperParser[wrappers.DoubleValue])
    .registerMessageFormatter[wrappers.FloatValue](primitiveWrapperWriter, primitiveWrapperParser[wrappers.FloatValue])
    .registerMessageFormatter[wrappers.Int32Value](primitiveWrapperWriter, primitiveWrapperParser[wrappers.Int32Value])
    .registerMessageFormatter[wrappers.Int64Value](primitiveWrapperWriter, primitiveWrapperParser[wrappers.Int64Value])
    .registerMessageFormatter[wrappers.UInt32Value](primitiveWrapperWriter, primitiveWrapperParser[wrappers.UInt32Value])
    .registerMessageFormatter[wrappers.UInt64Value](primitiveWrapperWriter, primitiveWrapperParser[wrappers.UInt64Value])
    .registerMessageFormatter[wrappers.BoolValue](primitiveWrapperWriter, primitiveWrapperParser[wrappers.BoolValue])
    .registerMessageFormatter[wrappers.BytesValue](primitiveWrapperWriter, primitiveWrapperParser[wrappers.BytesValue])
    .registerMessageFormatter[wrappers.StringValue](primitiveWrapperWriter, primitiveWrapperParser[wrappers.StringValue])
    .registerEnumFormatter[NullValue](
      (_, _) => JNull,
      (parser, value) =>
        value match {
          case JNull => NullValue.NULL_VALUE.scalaValueDescriptor
          case _     => parser.defaultEnumParser(NullValue.scalaDescriptor, value)
      }
    )
    .registerWriter[com.google.protobuf.struct.Value](StructFormat.structValueWriter, StructFormat.structValueParser)
    .registerWriter[com.google.protobuf.struct.Struct](StructFormat.structWriter, StructFormat.structParser)
    .registerWriter[com.google.protobuf.struct.ListValue](StructFormat.listValueWriter, StructFormat.listValueParser)
    .registerMessageFormatter[com.google.protobuf.any.Any](AnyFormat.anyWriter, AnyFormat.anyParser)

  def primitiveWrapperWriter[T <: GeneratedMessage with Message[T]](implicit cmp: GeneratedMessageCompanion[T]): ((Printer, T) => JValue) = {
    val fieldDesc = cmp.scalaDescriptor.findFieldByNumber(1).get
    (printer, t) =>
      printer.serializeSingleValue(fieldDesc, t.getField(fieldDesc), formattingLongAsNumber = true)
  }

  def primitiveWrapperParser[T <: GeneratedMessage with Message[T]](implicit cmp: GeneratedMessageCompanion[T]): ((Parser, JValue) => T) = {
    val fieldDesc = cmp.scalaDescriptor.findFieldByNumber(1).get
    (parser, jv) =>
      cmp.messageReads.read(
        PMessage(
          Map(
            fieldDesc -> JsonFormat.parsePrimitive(fieldDesc.scalaType,
                                                   fieldDesc.protoType,
                                                   jv,
                                                   throw new JsonFormatException(s"Unexpected value for ${cmp.scalaDescriptor.name}")))))
  }

  val printer = new FixedPrinter()
  val parser  = new Parser()

  def toJsonString[A <: GeneratedMessage](m: A): String = printer.print(m)

  def toJson[A <: GeneratedMessage](m: A): JValue = printer.toJson(m)

  def fromJson[A <: GeneratedMessage with Message[A]: GeneratedMessageCompanion](value: JValue): A = {
    parser.fromJson(value)
  }

  def fromJsonString[A <: GeneratedMessage with Message[A]: GeneratedMessageCompanion](str: String): A = {
    parser.fromJsonString(str)
  }

  implicit def protoToReader[T <: GeneratedMessage with Message[T]: GeneratedMessageCompanion]: Reader[T] =
    new Reader[T] {
      def read(value: JValue): T = parser.fromJson(value)
    }

  implicit def protoToWriter[T <: GeneratedMessage with Message[T]]: Writer[T] = new Writer[T] {
    def write(obj: T): JValue = printer.toJson(obj)
  }

  def defaultValue(fd: FieldDescriptor): PValue = {
    require(fd.isOptional)
    fd.scalaType match {
      case ScalaType.Int        => PInt(0)
      case ScalaType.Long       => PLong(0L)
      case ScalaType.Float      => PFloat(0)
      case ScalaType.Double     => PDouble(0)
      case ScalaType.Boolean    => PBoolean(false)
      case ScalaType.String     => PString("")
      case ScalaType.ByteString => PByteString(ByteString.EMPTY)
      case ScalaType.Enum(ed)   => PEnum(ed.values(0))
      case ScalaType.Message(_) => throw new RuntimeException("No default value for message")
    }
  }

  def parsePrimitive(scalaType: ScalaType, protoType: FieldDescriptorProto.Type, value: JValue, onError: => PValue): PValue =
    (scalaType, value) match {
      case (ScalaType.Int, JInt(x))                               => PInt(x.intValue)
      case (ScalaType.Int, JDouble(x))                            => PInt(x.intValue)
      case (ScalaType.Int, JDecimal(x))                           => PInt(x.intValue)
      case (ScalaType.Int, JString(x)) if protoType.isTypeInt32   => parseInt32(x)
      case (ScalaType.Int, JString(x)) if protoType.isTypeSint32  => parseInt32(x)
      case (ScalaType.Int, JString(x))                            => parseUint32(x)
      case (ScalaType.Long, JLong(x))                             => PLong(x.toLong)
      case (ScalaType.Long, JDecimal(x))                          => PLong(x.longValue())
      case (ScalaType.Long, JString(x)) if protoType.isTypeInt64  => parseInt64(x)
      case (ScalaType.Long, JString(x)) if protoType.isTypeSint64 => parseInt64(x)
      case (ScalaType.Long, JString(x))                           => parseUint64(x)
      case (ScalaType.Long, JInt(x))                              => PLong(x.toLong)
      case (ScalaType.Double, JDouble(x))                         => PDouble(x)
      case (ScalaType.Double, JInt(x))                            => PDouble(x.toDouble)
      case (ScalaType.Double, JDecimal(x))                        => PDouble(x.toDouble)
      case (ScalaType.Double, JString("NaN"))                     => PDouble(Double.NaN)
      case (ScalaType.Double, JString("Infinity"))                => PDouble(Double.PositiveInfinity)
      case (ScalaType.Double, JString("-Infinity"))               => PDouble(Double.NegativeInfinity)
      case (ScalaType.Float, JDouble(x))                          => PFloat(x.toFloat)
      case (ScalaType.Float, JInt(x))                             => PFloat(x.toFloat)
      case (ScalaType.Float, JDecimal(x))                         => PFloat(x.toFloat)
      case (ScalaType.Float, JString("NaN"))                      => PFloat(Float.NaN)
      case (ScalaType.Float, JString("Infinity"))                 => PFloat(Float.PositiveInfinity)
      case (ScalaType.Float, JString("-Infinity"))                => PFloat(Float.NegativeInfinity)
      case (ScalaType.Boolean, JBool(b))                          => PBoolean(b)
      case (ScalaType.String, JString(s))                         => PString(s)
      case (ScalaType.ByteString, JString(s)) =>
        PByteString(ByteString.copyFrom(Base58.decode(s).get))
      case _ => onError
    }

  def parseBigDecimal(value: String): BigDecimal = {
    try {
      // JSON doesn't distinguish between integer values and floating point values so "1" and
      // "1.000" are treated as equal in JSON. For this reason we accept floating point values for
      // integer fields as well as long as it actually is an integer (i.e., round(value) == value).
      BigDecimal(value)
    } catch {
      case e: Exception =>
        throw JsonFormatException(s"Not a numeric value: $value", e)
    }
  }

  def parseInt32(value: String): PValue = {
    try {
      PInt(value.toInt)
    } catch {
      case _: Exception =>
        try {
          PInt(parseBigDecimal(value).toIntExact)
        } catch {
          case e: Exception =>
            throw JsonFormatException(s"Not an int32 value: $value", e)
        }
    }
  }

  def parseInt64(value: String): PValue = {
    try {
      PLong(value.toLong)
    } catch {
      case _: Exception =>
        val bd = parseBigDecimal(value)
        try {
          PLong(bd.toLongExact)
        } catch {
          case e: Exception =>
            throw JsonFormatException(s"Not an int64 value: $value", e)
        }
    }
  }

  def parseUint32(value: String): PValue = {
    try {
      val result = value.toLong
      if (result < 0 || result > 0xFFFFFFFFl) throw new JsonFormatException(s"Out of range uint32 value: $value")
      return PInt(result.toInt)
    } catch {
      case e: JsonFormatException => throw e
      case e: Exception           => // Fall through.
    }
    parseBigDecimal(value).toBigIntExact().map { intVal =>
      if (intVal < 0 || intVal > 0xFFFFFFFFl) throw new JsonFormatException(s"Out of range uint32 value: $value")
      PLong(intVal.intValue())
    } getOrElse {
      throw new JsonFormatException(s"Not an uint32 value: $value")
    }
  }

  val MAX_UINT64 = BigInt("FFFFFFFFFFFFFFFF", 16)

  def parseUint64(value: String): PValue = {
    parseBigDecimal(value).toBigIntExact().map { intVal =>
      if (intVal < 0 || intVal > MAX_UINT64) {
        throw new JsonFormatException(s"Out of range uint64 value: $value")
      }
      PLong(intVal.longValue())
    } getOrElse {
      throw new JsonFormatException(s"Not an uint64 value: $value")
    }
  }

  def jsonName(fd: FieldDescriptor): String = {
    // protoc<3 doesn't know about json_name, so we fill it in if it's not populated.
    fd.asProto.jsonName.getOrElse(NameUtils.snakeCaseToCamelCase(fd.asProto.getName))
  }
}
