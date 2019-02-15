package com.wavesplatform.transaction.protobuf
import com.wavesplatform.common.state.ByteStr

final case class AssetId(bytes: ByteStr) extends AnyVal {
  def isWaves: Boolean = bytes.isEmpty
}

object AssetId {
  val Waves = AssetId(ByteStr.empty)

  implicit def fromBytes(bytes: ByteStr): AssetId = new AssetId(bytes)
  implicit def toBytes(assetId: AssetId): ByteStr = assetId.bytes
}
