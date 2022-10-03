package com.wavesplatform.ride.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script

import java.nio.charset.StandardCharsets

case class RunnerAssetInfo(
    issuerPublicKey: PublicKey = EmptyPublicKey,
    name: ByteStr = ByteStr("name".getBytes(StandardCharsets.UTF_8)),
    description: ByteStr = ByteStr("description".getBytes(StandardCharsets.UTF_8)),
    decimals: Int = 8,
    reissuable: Boolean = false,
    quantity: Long = 9007199254740991L, // In JS: MAX_SAFE_INTEGER
    script: Option[Script] = None,
    minSponsoredAssetFee: Long = 0L
)
