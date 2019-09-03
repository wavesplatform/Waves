package com.wavesplatform.lang.v1.repl.model

case class AssetInfo(
    assetId:              ByteString,
    quantity:             Long,
    decimals:             Int,
    issuer:               ByteString,
    reissuable:           Boolean,
    scripted:             Boolean,
    minSponsoredAssetFee: Long
) {
    def sponsored: Boolean = minSponsoredAssetFee != 0
}