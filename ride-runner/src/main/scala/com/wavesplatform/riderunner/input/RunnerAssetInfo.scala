package com.wavesplatform.riderunner.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.lang.script.Script

case class RunnerAssetInfo(
    issuerPublicKey: PublicKey = EmptyPublicKey,
    name: String = "name",
    description: String = "description",
    decimals: Int = 8,
    reissuable: Boolean = false,
    quantity: Long = 9007199254740991L, // In JS: MAX_SAFE_INTEGER
    script: Option[Script] = None,
    minSponsoredAssetFee: Long = 0L
)
