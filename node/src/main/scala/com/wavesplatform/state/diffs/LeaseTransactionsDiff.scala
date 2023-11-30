package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.lease.*

object LeaseTransactionsDiff {
  def lease(blockchain: Blockchain)(tx: LeaseTransaction): Either[ValidationError, StateSnapshot] =
    DiffsCommon
      .processLease(blockchain, tx.amount, tx.sender, tx.recipient, tx.fee.value, tx.id(), tx.id())

  def leaseCancel(blockchain: Blockchain, time: Long)(tx: LeaseCancelTransaction): Either[ValidationError, StateSnapshot] =
    DiffsCommon
      .processLeaseCancel(blockchain, tx.sender, tx.fee.value, time, tx.leaseId, tx.id())
}
