package com.wavesplatform.ride.input

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves

case class RunnerTransactionInfo(
    amount: Long = 1,
    assetId: Asset = Waves,
    fee: Long = 100_000,
    feeAssetId: Asset = Waves,
    recipient: Option[String] = None,
    attachment: String = "",
    timestamp: Long = System.currentTimeMillis(),
    version: Byte = 3,
    senderPublicKey: PublicKey = EmptyPublicKey,
    proofs: List[ByteStr] = Nil,
    height: Option[Int] = None
)
