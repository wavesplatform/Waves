package com.wavesplatform.serialization.protobuf

trait PBSerializable[T] {
  final type SerializedT = Array[Byte]
  def protoBytes(value: T): SerializedT
}

trait PBSerializableUnsigned[T] extends PBSerializable[T] {
  def protoBytesUnsigned(value: T): SerializedT
}

object PBSerializable {
  implicit class PBSerializableOps[+T: PBSerializable](value: T) {
    def protoBytes: Array[Byte] = implicitly[PBSerializable[T]].protoBytes(value)
  }

  implicit class PBSerializableUnsignedOps[+T: PBSerializableUnsigned](value: T) {
    def protoBytesUnsigned: Array[Byte] = implicitly[PBSerializableUnsigned[T]].protoBytesUnsigned(value)
  }
}