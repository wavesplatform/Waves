package com.wavesplatform.block.protobuf
import com.wavesplatform.serialization.protobuf.json.ProtobufJsonFormats

//noinspection TypeAnnotation
trait PBBlockJson {
  implicit val PBBlockJsonFormat = ProtobufJsonFormats.generatedMessageJsonFormat[Block]
}
