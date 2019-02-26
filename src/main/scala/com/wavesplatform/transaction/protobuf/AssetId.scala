package com.wavesplatform.transaction.protobuf
import com.wavesplatform.common.state.ByteStr

final case class AssetId(bytes: ByteStr) extends AnyVal {
  def isWaves: Boolean = bytes.isEmpty
}

object AssetId {
  val Waves = AssetId(ByteStr.empty)

  implicit def fromBytes(bytes: ByteStr): AssetId                                                        = new AssetId(bytes)
  implicit def fromByteArray(bytes: Array[Byte]): AssetId                                                = new AssetId(bytes)
  implicit def toBytes(assetId: AssetId): ByteStr                                                        = assetId.bytes
  implicit def toByteArray(assetId: AssetId): Array[Byte]                                                = assetId.bytes
  implicit def fromVanillaAssetIdOption(assetId: Option[com.wavesplatform.transaction.AssetId]): AssetId = assetId.fold(Waves)(fromBytes)
  implicit def toVanillaAssetIdOption(assetId: AssetId): Option[com.wavesplatform.transaction.AssetId] =
    if (assetId.isWaves) None else Some(assetId.bytes)
}
