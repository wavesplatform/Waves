package com.wavesplatform.state

import cats.Semigroup
import com.google.protobuf.ByteString
import com.wavesplatform.protobuf.ByteStringExt
import play.api.libs.json.{Json, OWrites, Writes}

case class AssetInfo(name: ByteString, description: ByteString, lastUpdatedAt: Height)

object AssetInfo {
  implicit val byteStrFormat: Writes[ByteString] = com.wavesplatform.utils.byteStrFormat.contramap(_.toByteStr)
  implicit val format: OWrites[AssetInfo]        = Json.writes[AssetInfo]
  implicit val semigroup: Semigroup[AssetInfo]   = (_, y) => y

  def apply(name: String, description: String, lastUpdatedAt: Height): AssetInfo =
    AssetInfo(ByteString.copyFromUtf8(name), ByteString.copyFromUtf8(description), lastUpdatedAt)
}
