package com.wavesplatform.ride.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script

case class RunnerAssetInfo(
    issuerPublicKey: PublicKey,
    name: ByteStr,
    description: ByteStr,
    decimals: Int,
    reissuable: Boolean,
    quantity: Long,
    script: Option[Script],
    minSponsoredAssetFee: Long
)
