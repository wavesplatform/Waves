package com.wavesplatform.lang.v1.repl.node.http.response.model

import com.wavesplatform.lang.v1.traits.DataType
import io.circe.{Decoder, DecodingFailure, HCursor}

private[node] case class DataEntry(key: String, value: Any, `type`: DataType)

private[node] object DataEntry {
  implicit val decoder: Decoder[DataEntry] = (c: HCursor) =>
    for {
      rawType <- c.downField("type").as[String]
      key     <- c.downField("key").as[String]
      v = c.downField("value")
      (value, resolvedType) <- rawType match {
        case "binary"  => v.as[ByteString] map (b => (b.byteStr, DataType.ByteArray))
        case "boolean" => v.as[Boolean] map ((_, DataType.Boolean))
        case "integer" => v.as[Long] map ((_, DataType.Long))
        case "string"  => v.as[String] map ((_, DataType.String))
        case t         => Left(DecodingFailure(s"Illegal data entry type: $t", Nil))
      }
    } yield DataEntry(key, value, resolvedType)
}
