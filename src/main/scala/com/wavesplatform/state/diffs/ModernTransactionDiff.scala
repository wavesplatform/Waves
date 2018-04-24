package com.wavesplatform.state.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.diffs.modern._
import com.wavesplatform.state.{Blockchain, Diff}
import scorex.transaction.modern.assets.{BurnTx, IssueTx, ReissueTx, TransferTx}
import scorex.transaction.modern.lease.{LeaseCancelTx, LeaseTx}
import scorex.transaction.modern.smart.SetScriptTx
import scorex.transaction.modern.{CreateAliasTx, DataTx, ModernTransaction}
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.UnsupportedTransactionType

object ModernTransactionDiff {
  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: ModernTransaction): Either[ValidationError, Diff] = {
    tx match {
      case lctx: LeaseCancelTx => LeaseTxDiff.leaseCancel(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(lctx)
      case catx: CreateAliasTx => CreateAliasTxDiff(currentBlockHeight)(catx)
      case dtx: DataTx         => DataTxDiff(blockchain, currentBlockHeight)(dtx)
      case btx: BurnTx         => AssetTxDiff.burn(blockchain, currentBlockHeight)(btx)
      case ltx: LeaseTx        => LeaseTxDiff.lease(blockchain, currentBlockHeight)(ltx)
      case rtx: ReissueTx      => AssetTxDiff.reissue(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
      case sstx: SetScriptTx   => SetScriptDiff(currentBlockHeight)(sstx)
      case ttx: TransferTx     => AssetTxDiff.transfer(blockchain, currentBlockHeight)(ttx)
      case itx: IssueTx        => AssetTxDiff.issue(currentBlockHeight)(itx)
      case _                   => Left(UnsupportedTransactionType)
    }
  }
}
