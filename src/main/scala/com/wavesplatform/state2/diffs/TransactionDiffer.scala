package com.wavesplatform.state2.diffs


import com.wavesplatform.features.Functionalities
import com.wavesplatform.state2.Diff
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError.UnsupportedTransactionType
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  def apply(fn: Functionalities, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(s: StateReader, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      t0 <- Signed.validateSignatures(tx)
      t1 <- CommonValidation.disallowTxFromFuture(fn, currentBlockTimestamp, t0)
      t2 <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, t1)
      t3 <- CommonValidation.disallowBeforeActivationTime(fn, t2)
      t4 <- CommonValidation.disallowDuplicateIds(s, fn, currentBlockHeight, t3)
      t5 <- CommonValidation.disallowSendingGreaterThanBalance(s, fn, currentBlockTimestamp, t4)
      diff <- t5 match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case ptx: PaymentTransaction => PaymentTransactionDiff(s, currentBlockHeight, fn, currentBlockTimestamp)(ptx)
        case itx: IssueTransaction => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
        case rtx: ReissueTransaction => AssetTransactionsDiff.reissue(s, fn, currentBlockTimestamp, currentBlockHeight)(rtx)
        case btx: BurnTransaction => AssetTransactionsDiff.burn(s, currentBlockHeight)(btx)
        case ttx: TransferTransaction => TransferTransactionDiff(s, fn, currentBlockTimestamp, currentBlockHeight)(ttx)
        case ltx: LeaseTransaction => LeaseTransactionsDiff.lease(s, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction => LeaseTransactionsDiff.leaseCancel(s, fn, currentBlockTimestamp, currentBlockHeight)(ltx)
        case etx: ExchangeTransaction => ExchangeTransactionDiff(s, currentBlockHeight)(etx)
        case atx: CreateAliasTransaction => CreateAliasTransactionDiff(currentBlockHeight)(atx)
        case _ => Left(UnsupportedTransactionType)
      }
      positiveDiff <- BalanceDiffValidation(s, currentBlockTimestamp, fn)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
