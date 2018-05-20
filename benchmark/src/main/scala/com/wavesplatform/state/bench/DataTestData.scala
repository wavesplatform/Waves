package com.wavesplatform.state.bench

import java.nio.charset.StandardCharsets

import com.wavesplatform.lang.v1.traits.DataType
import scodec._
import scodec.bits._
import scodec.codecs._

case class DataTestData(addr: ByteVector, key: String, dataType: DataType)
object DataTestData {
  implicit val dt: Discriminated[DataType, Int] = Discriminated[DataType, Int](uint8)
  implicit val dtCodec: Codec[DataType]         = mappedEnum(uint8, DataType.Boolean -> 0, DataType.ByteArray -> 1, DataType.Long -> 2, DataType.String -> 3)
  val codec: Codec[DataTestData] = {
    ("addr" | variableSizeBytes(uint8, bytes)) ::
      ("key" | variableSizeBytes(uint8, string(StandardCharsets.UTF_8))) ::
      ("dataType" | dtCodec)
  }.as[DataTestData]
}
