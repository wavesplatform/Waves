package com.wavesplatform.state

import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey

case class AssetDescription(
    originTransactionId: TransactionId,
    issuer: PublicKey,
    name: ByteString,
    description: ByteString,
    decimals: Int,
    reissuable: Boolean,
    totalVolume: BigInt,
    lastUpdatedAt: Height,
    script: Option[AssetScriptInfo],
    sponsorship: Long,
    nft: Boolean,
    sequenceInBlock: Int,
    issueHeight: Height
)
