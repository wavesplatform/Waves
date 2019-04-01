package com.wavesplatform.settings

import com.wavesplatform.settings.utils.ConfigSettingsValidator
import com.wavesplatform.transaction.assets.exchange.AssetPair
import net.ceedubs.ficus.readers.ValueReader

case class AllowedAssetPairsSettings(value: Set[AssetPair])

object AllowedAssetPairsSettings {

  implicit val allowedAssetPairsReader: ValueReader[AllowedAssetPairsSettings] = { (cfg, path) =>
    ConfigSettingsValidator(cfg)
      .validateList[AssetPair](path)
      .map(assetPairList => AllowedAssetPairsSettings(assetPairList.toSet))
      .valueOr(e => throw new Exception(e.mkString(", ")))
  }
}
