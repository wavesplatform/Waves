package com.wavesplatform.state.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.state.reader.SnapshotStateReader
import scorex.transaction.ValidationError.UnsupportedTransactionType
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.{SetScriptTransaction, Verifier}

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      s: SnapshotStateReader,
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    for {
      _ <- Verifier(s, currentBlockHeight)(tx)
      _ <- CommonValidation.disallowTxFromFuture(settings, currentBlockTimestamp, tx)
      _ <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, tx)
      _ <- CommonValidation.disallowBeforeActivationTime(blockchain, currentBlockHeight, tx)
      _ <- CommonValidation.disallowDuplicateIds(s, settings, currentBlockHeight, tx)
      _ <- CommonValidation.disallowSendingGreaterThanBalance(s, settings, currentBlockTimestamp, tx)
      diff <- tx match {
        case gtx: GenesisTransaction            => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case ptx: PaymentTransaction            => PaymentTransactionDiff(s, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
        case itx: IssueTransaction              => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
        case sitx: SmartIssueTransaction        => AssetTransactionsDiff.smartIssue(currentBlockHeight)(sitx)
        case rtx: ReissueTransaction            => AssetTransactionsDiff.reissue(s, settings, currentBlockTimestamp, currentBlockHeight, blockchain)(rtx)
        case btx: BurnTransaction               => AssetTransactionsDiff.burn(s, blockchain, currentBlockHeight)(btx)
        case ttx: TransferTransaction           => TransferTransactionDiff(s, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
        case mtx: MassTransferTransaction       => MassTransferTransactionDiff(s, currentBlockTimestamp, currentBlockHeight)(mtx)
        case ltx: LeaseTransaction              => LeaseTransactionsDiff.lease(s, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction        => LeaseTransactionsDiff.leaseCancel(s, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
        case etx: ExchangeTransaction           => ExchangeTransactionDiff(s, currentBlockHeight)(etx)
        case atx: CreateAliasTransaction        => CreateAliasTransactionDiff(currentBlockHeight)(atx)
        case dtx: DataTransaction               => DataTransactionDiff(s, currentBlockHeight)(dtx)
        case sstx: SetScriptTransaction         => SetScriptTransactionDiff(currentBlockHeight)(sstx)
        case sttx: VersionedTransferTransaction => ScriptTransferTransactionDiff(s, currentBlockHeight)(sttx)
        case _                                  => Left(UnsupportedTransactionType)
      }
      positiveDiff <- BalanceDiffValidation(s, currentBlockHeight, settings)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
