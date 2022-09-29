package com.wavesplatform.ride.input

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset

case class RunnerTransactionInfo(
    feeAssetId: Asset,
    amount: Long,
    assetId: Asset,
    recipient: AddressOrAlias,
    attachment: ByteStr,
    fee: Long,
    timestamp: Long,
    version: Byte,
    senderPublicKey: PublicKey,
    proofs: List[ByteStr],
    height: Int
)
