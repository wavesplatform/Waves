package com.wavesplatform.state.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.diffs.modern.{PaymentTransactionDiff, _}
import com.wavesplatform.state.{Blockchain, Diff}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.base._
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.UnsupportedTransactionType
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}

object ModernTransactionDiff {
  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    tx match {
      case gtx: GenesisTransaction          => GenesisTransactionDiff(currentBlockHeight)(gtx)
      case ptx: PaymentTransaction          => PaymentTransactionDiff(blockchain, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
      case etx: ExchangeTransaction         => ExchangeTransactionDiff(blockchain, currentBlockHeight)(etx)
      case itx: IssueTxBase                 => AssetTxDiff.issue(currentBlockHeight)(itx)
      case rtx: ReissueTxBase               => AssetTxDiff.reissue(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
      case btx: BurnTxBase                  => AssetTxDiff.burn(blockchain, currentBlockHeight)(btx)
      case ttx: TransferTxBase              => AssetTxDiff.transfer(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
      case mttx: MassTransferTxBase         => MassTranserTxDiff(blockchain, currentBlockTimestamp, currentBlockHeight)(mttx)
      case dtx: DataTxBase                  => DataTxDiff(blockchain, currentBlockHeight)(dtx)
      case ltx: LeaseTxBase                 => LeaseTxDiff.lease(blockchain, currentBlockHeight)(ltx)
      case lctx: LeaseCancelTxBase          => LeaseTxDiff.leaseCancel(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(lctx)
      case sftx: SponsorFeeTxBase           => AssetTxDiff.sponsor(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(sftx)
      case cstx: CancelFeeSponsorshipTxBase => AssetTxDiff.cancelSponsorship(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(cstx)
      case catx: CreateAliasTxBase          => CreateAliasTxDiff(currentBlockHeight)(catx)
      case sstx: SetScriptTxBase            => SetScriptDiff(currentBlockHeight)(sstx)
      case _                                => Left(UnsupportedTransactionType)
    }
  }
}
