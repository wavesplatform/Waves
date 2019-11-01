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

object TransactionDiffer {

  private val stats = TxProcessingStats

  import stats.TxTimerExt

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError {
    override def toString: String = s"TransactionValidationError(cause = $cause,\ntx = ${tx.toPrettyString})"
  }

  def apply(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    val func =
      if (verify) verified(prevBlockTimestamp, currentBlockTimestamp) _
      else unverified(currentBlockTimestamp) _
    func(blockchain, tx)
  }

  def verified(
      prevBlockTimestamp: Option[Long],
      currentBlockTimestamp: Long
  )(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Diff] = {
    for {
      _ <- Verifier(blockchain)(tx)
      _ <- TracedResult(
        stats.commonValidation
          .measureForType(tx.typeId) {
            for {
              _ <- CommonValidation.disallowTxFromFuture(blockchain.settings.functionalitySettings, currentBlockTimestamp, tx)
              _ <- CommonValidation.disallowTxFromPast(blockchain.settings.functionalitySettings, prevBlockTimestamp, tx)
              _ <- CommonValidation.disallowBeforeActivationTime(blockchain, tx)
              _ <- CommonValidation.disallowDuplicateIds(blockchain, tx)
              _ <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, currentBlockTimestamp, tx)
              _ <- FeeValidation(blockchain, tx)
            } yield ()
          }
      )
      diff <- unverified(currentBlockTimestamp)(blockchain, tx)
      positiveDiff <- stats.balanceValidation
        .measureForType(tx.typeId) {
          BalanceDiffValidation(blockchain)(diff)
        }
    } yield positiveDiff
  }.leftMap(TransactionValidationError(_, tx))

  private def unverified(currentBlockTimestamp: Long)(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Diff] =
    stats.transactionDiffValidation.measureForType(tx.typeId) {
      tx match {
        case gtx: GenesisTransaction     => GenesisTransactionDiff(blockchain.height)(gtx)
        case ptx: PaymentTransaction     => PaymentTransactionDiff(blockchain)(ptx)
        case ci: InvokeScriptTransaction => InvokeScriptTransactionDiff(blockchain)(ci)
        case etx: ExchangeTransaction    => ExchangeTransactionDiff(blockchain)(etx)
        case otherTx: ProvenTransaction =>
          unverifiedWithEstimate(currentBlockTimestamp)(blockchain, otherTx)
            .map(complexityDiff(blockchain, otherTx) |+| _)
        case _ => Left(UnsupportedTransactionType)
      }
    }

  private def complexityDiff(blockchain: Blockchain, tx: ProvenTransaction): Diff =
    Diff.empty.copy(scriptsComplexity = DiffsCommon.getScriptsComplexity(blockchain, tx))

  private def unverifiedWithEstimate(
      currentBlockTimestamp: Long
  )(blockchain: Blockchain, tx: ProvenTransaction): TracedResult[ValidationError, Diff] =
    tx match {
      case itx: IssueTransaction           => AssetTransactionsDiff.issue(blockchain)(itx)
      case rtx: ReissueTransaction         => AssetTransactionsDiff.reissue(blockchain, currentBlockTimestamp)(rtx)
      case btx: BurnTransaction            => AssetTransactionsDiff.burn(blockchain)(btx)
      case ttx: TransferTransaction        => TransferTransactionDiff(blockchain, currentBlockTimestamp)(ttx)
      case mtx: MassTransferTransaction    => MassTransferTransactionDiff(blockchain, currentBlockTimestamp)(mtx)
      case ltx: LeaseTransaction           => LeaseTransactionsDiff.lease(blockchain)(ltx)
      case ltx: LeaseCancelTransaction     => LeaseTransactionsDiff.leaseCancel(blockchain, currentBlockTimestamp)(ltx)
      case atx: CreateAliasTransaction     => CreateAliasTransactionDiff(blockchain)(atx)
      case dtx: DataTransaction            => DataTransactionDiff(blockchain)(dtx)
      case sstx: SetScriptTransaction      => SetScriptTransactionDiff(blockchain)(sstx)
      case sstx: SetAssetScriptTransaction => AssetTransactionsDiff.setAssetScript(blockchain, currentBlockTimestamp)(sstx)
      case stx: SponsorFeeTransaction      => AssetTransactionsDiff.sponsor(blockchain, currentBlockTimestamp)(stx)
      case _                               => Left(UnsupportedTransactionType)
    }
}
