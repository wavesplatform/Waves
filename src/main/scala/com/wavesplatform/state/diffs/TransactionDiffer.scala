package com.wavesplatform.state.diffs

import com.wavesplatform.metrics._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.ValidationError.UnsupportedTransactionType
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction, Verifier}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.ScorexLogging

object TransactionDiffer extends Instrumented with ScorexLogging {

  private val stats = TxProcessingStats

  import stats.TxTimerExt

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings,
            prevBlockTimestamp: Option[Long],
            currentBlockTimestamp: Long,
            currentBlockHeight: Int,
            verify: Boolean = true)(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Diff] = {
    val func =
      if (verify) verified(settings, prevBlockTimestamp, currentBlockTimestamp, currentBlockHeight) _
      else unverified(settings, currentBlockTimestamp, currentBlockHeight) _
    func(blockchain, tx)
  }

  def verified(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    for {
      _ <- Verifier(blockchain, currentBlockHeight)(tx)
      _ <- stats.commonValidation
        .measureForType(tx.builder.typeId) {
          for {
            _ <- CommonValidation.disallowTxFromFuture(settings, currentBlockTimestamp, tx)
            _ <- CommonValidation.disallowTxFromPast(settings, prevBlockTimestamp, tx)
            _ <- CommonValidation.disallowBeforeActivationTime(blockchain, currentBlockHeight, tx)
            _ <- CommonValidation.disallowDuplicateIds(blockchain, settings, currentBlockHeight, tx)
            _ <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, settings, currentBlockTimestamp, tx)
            _ <- CommonValidation.checkFee(blockchain, settings, currentBlockHeight, tx)
          } yield ()
        }
      diff <- unverified(settings, currentBlockTimestamp, currentBlockHeight)(blockchain, tx)
      positiveDiff <- stats.balanceValidation
        .measureForType(tx.builder.typeId) {
          BalanceDiffValidation(blockchain, currentBlockHeight, settings)(diff)
        }
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))

  def unverified(settings: FunctionalitySettings, currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    stats.transactionDiffValidation.measureForType(tx.builder.typeId) {
      tx match {
        case gtx: GenesisTransaction      => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case ptx: PaymentTransaction      => PaymentTransactionDiff(blockchain, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
        case itx: IssueTransaction        => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
        case rtx: ReissueTransaction      => AssetTransactionsDiff.reissue(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
        case btx: BurnTransaction         => AssetTransactionsDiff.burn(blockchain, currentBlockHeight)(btx)
        case ttx: TransferTransaction     => TransferTransactionDiff(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
        case mtx: MassTransferTransaction => MassTransferTransactionDiff(blockchain, currentBlockTimestamp, currentBlockHeight)(mtx)
        case ltx: LeaseTransaction        => LeaseTransactionsDiff.lease(blockchain, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction  => LeaseTransactionsDiff.leaseCancel(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
        case etx: ExchangeTransaction     => ExchangeTransactionDiff(blockchain, currentBlockHeight)(etx)
        case atx: CreateAliasTransaction  => CreateAliasTransactionDiff(blockchain, currentBlockHeight)(atx)
        case dtx: DataTransaction         => DataTransactionDiff(blockchain, currentBlockHeight)(dtx)
        case sstx: SetScriptTransaction   => SetScriptTransactionDiff(blockchain, currentBlockHeight)(sstx)
        case sstx: SetAssetScriptTransaction =>
          AssetTransactionsDiff.setAssetScript(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(sstx)
        case stx: SponsorFeeTransaction  => AssetTransactionsDiff.sponsor(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(stx)
        case ci: InvokeScriptTransaction => InvokeScriptTransactionDiff.apply(blockchain, currentBlockHeight)(ci)
        case _                           => Left(UnsupportedTransactionType)
      }
    }
  }
}
