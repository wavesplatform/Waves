package com.wavesplatform.lang.v1.repl.http.response.model

private[http] case class AssetInfoResponse(
    assetId:              ByteString,
    quantity:             Long,
    decimals:             Int,
    issuer:               ByteString,
    reissuable:           Boolean,
    scripted:             Boolean,
    minSponsoredAssetFee: Option[Long]
) {
    def sponsored: Boolean = minSponsoredAssetFee.nonEmpty
}
