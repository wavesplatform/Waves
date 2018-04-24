package com.wavesplatform.state.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.diffs.legacy._
import com.wavesplatform.state.{Blockchain, Diff}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.UnsupportedTransactionType

object LegacyTransactionDiff {
  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    tx match {
      case gtx: GenesisTransaction     => GenesisTransactionDiff(currentBlockHeight)(gtx)
      case ptx: PaymentTransaction     => PaymentTransactionDiff(blockchain, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
      case itx: IssueTransaction       => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
      case sitx: SmartIssueTransaction => AssetTransactionsDiff.smartIssue(currentBlockHeight)(sitx)
      case rtx: ReissueTransaction =>
        AssetTransactionsDiff.reissue(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
      case btx: BurnTransaction               => AssetTransactionsDiff.burn(blockchain, currentBlockHeight)(btx)
      case ttx: TransferTransaction           => TransferTransactionDiff(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
      case mtx: MassTransferTransaction       => MassTransferTransactionDiff(blockchain, currentBlockTimestamp, currentBlockHeight)(mtx)
      case ltx: LeaseTransaction              => LeaseTransactionsDiff.lease(blockchain, currentBlockHeight)(ltx)
      case ltx: LeaseCancelTransaction        => LeaseTransactionsDiff.leaseCancel(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
      case etx: ExchangeTransaction           => ExchangeTransactionDiff(blockchain, currentBlockHeight)(etx)
      case atx: CreateAliasTransaction        => CreateAliasTransactionDiff(currentBlockHeight)(atx)
      case dtx: DataTransaction               => DataTransactionDiff(blockchain, currentBlockHeight)(dtx)
      case sstx: SetScriptTransaction         => SetScriptTransactionDiff(currentBlockHeight)(sstx)
      case sttx: VersionedTransferTransaction => ScriptTransferTransactionDiff(blockchain, currentBlockHeight)(sttx)
      case _                                  => Left(UnsupportedTransactionType)
    }
  }
}
