package com.wavesplatform.state2.diffs


import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.Diff
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.transaction.ValidationError.UnsupportedTransactionType
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError,tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(s: SnapshotStateReader, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      t0 <- tx.signaturesValid()
      t1 <- CommonValidation.disallowTxFromFuture(settings, currentBlockTimestamp, t0)
      t2 <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, t1)
      t3 <- CommonValidation.disallowBeforeActivationTime(settings, t2)
      t4 <- CommonValidation.disallowDuplicateIds(s, settings, currentBlockHeight, t3)
      t5 <- CommonValidation.disallowSendingGreaterThanBalance(s, settings, currentBlockTimestamp, t4)
      diff <- t5 match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case ptx: PaymentTransaction => PaymentTransactionDiff(s, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
        case itx: IssueTransaction => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
        case rtx: ReissueTransaction => AssetTransactionsDiff.reissue(s, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
        case btx: BurnTransaction => AssetTransactionsDiff.burn(s, currentBlockHeight)(btx)
        case ttx: TransferTransaction => TransferTransactionDiff(s, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
        case ltx: LeaseTransaction => LeaseTransactionsDiff.lease(s, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction => LeaseTransactionsDiff.leaseCancel(s, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
        case etx: ExchangeTransaction => ExchangeTransactionDiff(s, currentBlockHeight)(etx)
        case atx: CreateAliasTransaction => CreateAliasTransactionDiff(currentBlockHeight)(atx)
        case _ => Left(UnsupportedTransactionType)
      }
      positiveDiff <- BalanceDiffValidation(s, currentBlockTimestamp, settings)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
