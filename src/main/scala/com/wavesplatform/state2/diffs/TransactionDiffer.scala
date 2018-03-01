package com.wavesplatform.state2.diffs


import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.transaction.ValidationError.UnsupportedTransactionType
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.{SetScriptTransaction, Verifier}

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)
           (s: SnapshotStateReader, fp: FeatureProvider, tx: Transaction): Either[ValidationError, Diff] = {
    for {
      t0 <- Verifier(s, currentBlockHeight)(tx)
      t1 <- CommonValidation.disallowTxFromFuture(settings, currentBlockTimestamp, t0)
      t2 <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, t1)
      t3 <- CommonValidation.disallowBeforeActivationTime(fp, currentBlockHeight, t2)
      t4 <- CommonValidation.disallowDuplicateIds(s, settings, currentBlockHeight, t3)
      t5 <- CommonValidation.disallowSendingGreaterThanBalance(s, settings, currentBlockTimestamp, t3)
      diff <- t4 match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case ptx: PaymentTransaction => PaymentTransactionDiff(s, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
        case itx: IssueTransaction => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
        case rtx: ReissueTransaction => AssetTransactionsDiff.reissue(s, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
        case btx: BurnTransaction => AssetTransactionsDiff.burn(s, currentBlockHeight)(btx)
        case ttx: TransferTransaction => TransferTransactionDiff(s, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
        case mtx: MassTransferTransaction => MassTransferTransactionDiff(s, currentBlockTimestamp, currentBlockHeight)(mtx)
        case ltx: LeaseTransaction => LeaseTransactionsDiff.lease(s, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction => LeaseTransactionsDiff.leaseCancel(s, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
        case etx: ExchangeTransaction => ExchangeTransactionDiff(s, currentBlockHeight)(etx)
        case atx: CreateAliasTransaction => CreateAliasTransactionDiff(currentBlockHeight)(atx)
        case sstx: SetScriptTransaction => SetScriptTransactionDiff(currentBlockHeight)(sstx)
        case sttx: ScriptTransferTransaction => ScriptTransferTransactionDiff(s, currentBlockHeight)(sttx)
        case _ => Left(UnsupportedTransactionType)
      }
      positiveDiff <- BalanceDiffValidation(s, currentBlockHeight, settings)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
