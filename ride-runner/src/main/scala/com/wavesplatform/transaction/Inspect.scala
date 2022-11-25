package com.wavesplatform.transaction

import com.wavesplatform.transaction.transfer.TransferTransactionLike

object Inspect {
  def isTransferLike(tx: Transaction): Boolean = tx match {
    case _: TransferTransactionLike => true
    case etx: EthereumTransaction =>
      etx.payload match {
        case _: EthereumTransaction.Transfer => true
        case _                               => false
      }
    case _ => false
  }
}
