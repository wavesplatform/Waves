package com.wavesplatform.state

import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr

case class AssetDescription(
    originTransactionId: ByteStr,
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
