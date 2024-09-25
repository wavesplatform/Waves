package com.wavesplatform.lang.v1.repl.node.http.response.model

private[node] case class AssetInfoResponse(
    assetId: ByteString,
    name: String,
    description: String,
    quantity: Long,
    decimals: Int,
    issuer: ByteString,
    issuerPublicKey: ByteString,
    reissuable: Boolean,
    scripted: Boolean,
    minSponsoredAssetFee: Option[Long]
)
