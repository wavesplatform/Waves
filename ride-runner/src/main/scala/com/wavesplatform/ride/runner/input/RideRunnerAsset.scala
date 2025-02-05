package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.lang.script.Script

import java.nio.charset.StandardCharsets

case class RideRunnerAsset(
    issuerPublicKey: PublicKey = EmptyPublicKey,
    name: StringOrBytesAsByteArray = RideRunnerAsset.DefaultName,
    description: StringOrBytesAsByteArray = RideRunnerAsset.DefaultDescription,
    decimals: Int = 8,
    reissuable: Boolean = false,
    quantity: Long = 9007199254740991L, // In JS: MAX_SAFE_INTEGER
    script: Option[Script] = None,
    minSponsoredAssetFee: Long = 0L
)

object RideRunnerAsset {
  val DefaultName        = StringOrBytesAsByteArray("name".getBytes(StandardCharsets.UTF_8))
  val DefaultDescription = StringOrBytesAsByteArray("description".getBytes(StandardCharsets.UTF_8))
}
