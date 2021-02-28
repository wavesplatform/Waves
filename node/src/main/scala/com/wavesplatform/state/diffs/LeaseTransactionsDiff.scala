package com.wavesplatform.state.diffs

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.lease._

object LeaseTransactionsDiff {
  def lease(blockchain: Blockchain)(tx: LeaseTransaction): Either[ValidationError, Diff] =
    DiffsCommon
      .processLease(blockchain, tx.amount, tx.sender, tx.recipient, tx.fee, tx.id.value(), tx.id.value())
      .map(_.bindTransaction(tx).copy(scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)))

  def leaseCancel(blockchain: Blockchain, time: Long)(tx: LeaseCancelTransaction): Either[ValidationError, Diff] =
    DiffsCommon
      .processLeaseCancel(blockchain, tx.sender, tx.fee, time, tx.leaseId, tx.leaseId)
      .map(_.bindTransaction(tx).copy(scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)))
}
