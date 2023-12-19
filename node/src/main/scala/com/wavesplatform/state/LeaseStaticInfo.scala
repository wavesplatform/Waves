package com.wavesplatform.state

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.TxPositiveAmount

case class LeaseStaticInfo(
    sender: PublicKey,
    recipientAddress: Address,
    amount: TxPositiveAmount,
    sourceId: ByteStr,
    height: Int
)
