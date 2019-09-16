package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxValidationError.UnsupportedTransactionType
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction, Verifier}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.ScorexLogging

object TransactionDiffer extends ScorexLogging {

  private val stats = TxProcessingStats

  import stats.TxTimerExt

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError {
    override def toString: String = s"TransactionValidationError(cause = $cause,\ntx = ${tx.toPrettyString})"
  }

  def apply(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    val func =
      if (verify) verified(prevBlockTimestamp, currentBlockTimestamp, currentBlockHeight) _
      else unverified(currentBlockTimestamp, currentBlockHeight) _
    func(blockchain, tx)
  }

  def verified(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    for {
      _ <- Verifier(blockchain, currentBlockHeight)(tx)
      _ <- TracedResult(
        stats.commonValidation
          .measureForType(tx.builder.typeId) {
            for {
              _ <- CommonValidation.disallowTxFromFuture(blockchain.settings.functionalitySettings, currentBlockTimestamp, tx)
              _ <- CommonValidation.disallowTxFromPast(blockchain.settings.functionalitySettings, prevBlockTimestamp, tx)
              _ <- CommonValidation.disallowBeforeActivationTime(blockchain, currentBlockHeight, tx)
              _ <- CommonValidation.disallowDuplicateIds(blockchain, currentBlockHeight, tx)
              _ <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, currentBlockTimestamp, tx)
              _ <- FeeValidation(blockchain, currentBlockHeight, tx)
            } yield ()
          }
      )
      diff <- unverified(currentBlockTimestamp, currentBlockHeight)(blockchain, tx)
      positiveDiff <- stats.balanceValidation
        .measureForType(tx.builder.typeId) {
          BalanceDiffValidation(blockchain, currentBlockHeight, blockchain.settings.functionalitySettings)(diff)
        }
    } yield positiveDiff
  }.leftMap(TransactionValidationError(_, tx))

  private def unverified(
      currentBlockTimestamp: Long,
      currentBlockHeight: Int
  )(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Diff] =
    stats.transactionDiffValidation.measureForType(tx.builder.typeId) {
      tx match {
        case gtx: GenesisTransaction => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case ptx: PaymentTransaction =>
          PaymentTransactionDiff(blockchain.settings.functionalitySettings, currentBlockHeight, currentBlockTimestamp)(ptx)
        case ci: InvokeScriptTransaction => InvokeScriptTransactionDiff(blockchain, currentBlockHeight)(ci)
        case etx: ExchangeTransaction    => ExchangeTransactionDiff(blockchain, currentBlockHeight)(etx)
        case otherTx: ProvenTransaction  =>
          unverifiedWithEstimate(currentBlockTimestamp, currentBlockHeight)(blockchain, otherTx)
            .map(complexityDiff(currentBlockHeight, blockchain, otherTx) |+| _)
        case _  => Left(UnsupportedTransactionType)
      }
    }

  private def complexityDiff(height: Int, blockchain: Blockchain, tx: ProvenTransaction): Diff = {
    val complexity = DiffsCommon.getScriptsComplexity(blockchain, tx)
    Diff(height, tx, scriptsComplexity = complexity)
  }

  private def unverifiedWithEstimate(
    currentBlockTimestamp: Long,
    currentBlockHeight:    Int
  )(blockchain: Blockchain, tx: ProvenTransaction): TracedResult[ValidationError, Diff] =
      tx match {
        case itx: IssueTransaction           => AssetTransactionsDiff.issue(blockchain, currentBlockHeight)(itx)
        case rtx: ReissueTransaction         => AssetTransactionsDiff.reissue(blockchain, currentBlockHeight, currentBlockTimestamp)(rtx)
        case btx: BurnTransaction            => AssetTransactionsDiff.burn(blockchain, currentBlockHeight)(btx)
        case ttx: TransferTransaction        => TransferTransactionDiff(blockchain, currentBlockHeight, currentBlockTimestamp)(ttx)
        case mtx: MassTransferTransaction    => MassTransferTransactionDiff(blockchain, currentBlockTimestamp, currentBlockHeight)(mtx)
        case ltx: LeaseTransaction           => LeaseTransactionsDiff.lease(blockchain, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction     => LeaseTransactionsDiff.leaseCancel(blockchain, currentBlockTimestamp, currentBlockHeight)(ltx)
        case atx: CreateAliasTransaction     => CreateAliasTransactionDiff(blockchain, currentBlockHeight)(atx)
        case dtx: DataTransaction            => DataTransactionDiff(blockchain, currentBlockHeight)(dtx)
        case sstx: SetScriptTransaction      => SetScriptTransactionDiff(blockchain, currentBlockHeight)(sstx)
        case sstx: SetAssetScriptTransaction => AssetTransactionsDiff.setAssetScript(blockchain, currentBlockHeight, currentBlockTimestamp)(sstx)
        case stx: SponsorFeeTransaction      => AssetTransactionsDiff.sponsor(blockchain, currentBlockHeight, currentBlockTimestamp)(stx)
        case _                               => Left(UnsupportedTransactionType)
      }
}
