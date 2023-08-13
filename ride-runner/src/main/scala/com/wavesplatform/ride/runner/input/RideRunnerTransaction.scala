package com.wavesplatform.ride.runner.input

import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves

case class RideRunnerTransaction(
    amount: Long = 1,
    assetId: Asset = Waves,
    fee: Long = 100_000,
    feeAssetId: Asset = Waves,
    recipient: AddressOrAlias,
    senderPublicKey: PublicKey = EmptyPublicKey,
    height: Option[Int] = None,
    timestamp: Long = System.currentTimeMillis(),
    proofs: List[StringOrBytesAsByteArray] = Nil,
    version: Byte = 3,
    attachment: StringOrBytesAsByteArray = StringOrBytesAsByteArray(Array.empty[Byte])
)
