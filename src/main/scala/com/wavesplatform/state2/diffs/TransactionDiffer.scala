package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.Diff
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.ValidationError.UnsupportedTransactionType
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

object TransactionDiffer {

  case class TransactionValidationError(tx: Transaction, cause: ValidationError) extends ValidationError

  def apply(settings: FunctionalitySettings, time: Long, height: Int)(s: StateReader, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      t0 <- Signed.validateSignatures(tx)
      t1 <- CommonValidation.disallowTxFromFuture(s, settings, time, t0)
      t2 <- CommonValidation.disallowBeforeActivationTime(s, settings, t1)
      t3 <- CommonValidation.disallowDuplicateIds(s, settings, height, t2)
      t4 <- CommonValidation.disallowSendingGreaterThanBalance(s, settings, time, t3)
      diff <- t4 match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(height)(gtx)
        case ptx: PaymentTransaction => PaymentTransactionDiff(s, height, settings, time)(ptx)
        case itx: IssueTransaction => AssetTransactionsDiff.issue(s, height)(itx)
        case rtx: ReissueTransaction => AssetTransactionsDiff.reissue(s, settings, time, height)(rtx)
        case btx: BurnTransaction => AssetTransactionsDiff.burn(s, height)(btx)
        case ttx: TransferTransaction => TransferTransactionDiff(s, settings, time, height)(ttx)
        case ltx: LeaseTransaction => LeaseTransactionsDiff.lease(s, height)(ltx)
        case ltx: LeaseCancelTransaction => LeaseTransactionsDiff.leaseCancel(s, settings, time, height)(ltx)
        case etx: ExchangeTransaction => ExchangeTransactionDiff(s, height)(etx)
        case atx: CreateAliasTransaction => CreateAliasTransactionDiff(height)(atx)
        case atx: MakeAssetNameUniqueTransaction => AssetTransactionsDiff.makeAssetNameUnique(s, height)(atx)
        case t => Left(UnsupportedTransactionType)
      }
      positiveDiff <- BalanceDiffValidation(s, time, settings)(tx, diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(tx, _))
}
