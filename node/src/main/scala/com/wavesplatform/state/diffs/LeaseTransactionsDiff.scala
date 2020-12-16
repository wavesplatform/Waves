package com.wavesplatform.state.diffs

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.traits.domain.{Lease, LeaseCancel, Recipient}
import com.wavesplatform.state._
import com.wavesplatform.transaction.lease._

object LeaseTransactionsDiff {
  def lease(blockchain: Blockchain)(tx: LeaseTransaction): Either[ValidationError, Diff] =
    for {
      recipient <- blockchain.resolveAlias(tx.recipient)
      recipientAddress = Recipient.Address(ByteStr(recipient.bytes))
      r <- DiffsCommon.processLease(blockchain, tx.sender.toAddress, tx.fee, tx.id.value(), Lease(recipientAddress, tx.amount))
    } yield r.bindTransaction(tx).copy(scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx))

  def leaseCancel(blockchain: Blockchain, time: Long)(tx: LeaseCancelTransaction): Either[ValidationError, Diff] =
    DiffsCommon
      .processLeaseCancel(blockchain, tx.sender.toAddress, tx.fee, time, LeaseCancel(tx.leaseId))
      .map(_.bindTransaction(tx).copy(scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)))
}
