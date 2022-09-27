package com.wavesplatform.ride.input

import com.wavesplatform.state.TxMeta
import com.wavesplatform.transaction.transfer.TransferTransactionLike

case class RunnerTransactionInfo(
    transaction: Option[TransferTransactionLike] = None,
    meta: Option[TxMeta] = None
) {
  require(transaction.nonEmpty || meta.nonEmpty, s"transaction.nonEmpty=${transaction.nonEmpty} || meta.nonEmpty=${meta.nonEmpty}")
}
