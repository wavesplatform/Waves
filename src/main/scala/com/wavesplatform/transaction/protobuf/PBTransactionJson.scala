package com.wavesplatform.transaction.protobuf
import com.wavesplatform.serialization.protobuf.json.ProtobufJsonFormats

//noinspection TypeAnnotation
trait PBTransactionJson {
  implicit val PBTransactionJsonFormat = ProtobufJsonFormats.generatedMessageJsonFormat[Transaction]
}
