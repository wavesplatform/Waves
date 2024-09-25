package com.wavesplatform.state.bench

import java.nio.charset.StandardCharsets

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.traits.DataType
import scodec.{Codec, ValueCodecEnrichedWithHListSupport, HListCodecEnrichedWithHListSupport, TransformSyntax}
import scodec.bits.*
import scodec.codecs.*

case class DataTestData(addr: ByteStr, key: String, dataType: DataType)

object DataTestData {

  implicit val dt: Discriminated[DataType, Int] = Discriminated[DataType, Int](uint8)
  implicit val dtCodec: Codec[DataType] = mappedEnum(uint8, DataType.Boolean -> 0, DataType.ByteArray -> 1, DataType.Long -> 2, DataType.String -> 3)
  implicit val byteStrCodec: Codec[ByteStr] = bits.contramap[ByteStr](byteStr => BitVector(byteStr.arr)).encodeOnly

  val codec: Codec[DataTestData] = {
    ("addr" | variableSizeBytes(uint8, byteStrCodec)) ::
      ("key" | variableSizeBytes(uint8, string(StandardCharsets.UTF_8))) ::
      ("dataType" | dtCodec)
  }.as[DataTestData]

}
