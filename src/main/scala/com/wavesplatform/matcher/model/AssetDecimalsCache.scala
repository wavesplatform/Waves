package com.wavesplatform.matcher.model

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.AssetId

class AssetDecimalsCache(blockchain: Blockchain) {

  private val WavesDecimals      = 8
  private val assetDecimalsCache = new ConcurrentHashMap[Option[AssetId], Int](1000, 0.9f, 10)

  def get(asset: Option[AssetId]): Int = {
    asset.fold { WavesDecimals } { issuedAsset =>
      Option(assetDecimalsCache.get(asset)) getOrElse {

        val assetDecimals =
          blockchain
            .assetDescription(issuedAsset)
            .map(_.decimals)
            .getOrElse { throw new Exception("Can not get asset decimals since asset not found!") }

        assetDecimalsCache.put(Some(issuedAsset), assetDecimals)
        assetDecimals
      }
    }
  }
}
