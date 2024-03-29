package com.wavesplatform.state

import cats.kernel.Monoid
import play.api.libs.json.{Json, OFormat}

case class AssetVolumeInfo(isReissuable: Boolean, volume: BigInt)

object AssetVolumeInfo {
  implicit val format: OFormat[AssetVolumeInfo] = Json.format
  implicit val assetInfoMonoid: Monoid[AssetVolumeInfo] = new Monoid[AssetVolumeInfo] {
    override def empty: AssetVolumeInfo = AssetVolumeInfo(isReissuable = true, 0)
    override def combine(x: AssetVolumeInfo, y: AssetVolumeInfo): AssetVolumeInfo =
      AssetVolumeInfo(x.isReissuable && y.isReissuable, x.volume + y.volume)
  }
}
