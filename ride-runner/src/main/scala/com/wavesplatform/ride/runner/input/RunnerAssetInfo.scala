package com.wavesplatform.ride.runner.input

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.account.PublicKey
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.lang.script.Script

import java.nio.charset.StandardCharsets

case class RunnerAssetInfo(
    issuerPublicKey: PublicKey = EmptyPublicKey,
    name: StringOrBytesAsByteString = RunnerAssetInfo.DefaultName,
    description: StringOrBytesAsByteString = RunnerAssetInfo.DefaultDescription,
    decimals: Int = 8,
    reissuable: Boolean = false,
    quantity: Long = 9007199254740991L, // In JS: MAX_SAFE_INTEGER
    script: Option[Script] = None,
    minSponsoredAssetFee: Long = 0L
)

object RunnerAssetInfo {
  val DefaultName        = StringOrBytesAsByteString(UnsafeByteOperations.unsafeWrap("name".getBytes(StandardCharsets.UTF_8)))
  val DefaultDescription = StringOrBytesAsByteString(UnsafeByteOperations.unsafeWrap("description".getBytes(StandardCharsets.UTF_8)))
}
