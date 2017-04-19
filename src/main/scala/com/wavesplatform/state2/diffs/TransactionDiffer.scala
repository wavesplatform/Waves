package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.{Diff, EqByteArray}
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError.UnsupportedTransactionType
import scorex.transaction._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

object TransactionDiffer {
  def apply(settings: FunctionalitySettings, time: Long, height: Int)(s: StateReader, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      t0 <- CommonValidation.disallowTxFromFuture(s, settings, time, tx)
      t1 <- CommonValidation.disallowBeforeActivationTime(s, settings, t0)
      t2 <- CommonValidation.disallowDuplicateIds(s, settings, height, t1)
      t3 <- CommonValidation.disallowSendingGreaterThanBalance(s, settings, time, t2)
      diff <- t3 match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(height)(gtx)
        case ptx: PaymentTransaction => PaymentTransactionDiff(s, height, settings, time)(ptx)
        case itx: IssueTransaction => AssetTransactionsDiff.issue(s, height)(itx)
        case rtx: ReissueTransaction => AssetTransactionsDiff.reissue(s, settings,time, height)(rtx)
        case btx: BurnTransaction => AssetTransactionsDiff.burn(s, height)(btx)
        case ttx: TransferTransaction => TransferTransactionDiff(s, settings, time, height)(ttx)
        case ltx: LeaseTransaction => LeaseTransactionsDiff.lease(s, height)(ltx)
        case ltx: LeaseCancelTransaction => LeaseTransactionsDiff.leaseCancel(s, settings, time, height)(ltx)
        case etx: ExchangeTransaction => ExchangeTransactionDiff(s, height)(etx)
        case atx: CreateAliasTransaction => CreateAliasTransactionDiff(height)(atx)
        case t => Left(UnsupportedTransactionType(t))
      }
      positiveDiff <- BalanceDiffValidation(s, time)(tx, diff)
    } yield positiveDiff
  }
}