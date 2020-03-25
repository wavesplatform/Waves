package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction, Verifier}
import com.wavesplatform.transaction.transfer._
import play.api.libs.json.Json

import scala.util.Right

object TransactionDiffer {
  import TxProcessingStats.TxTimerExt

  private val stats = TxProcessingStats

  def apply(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] =
    validate(prevBlockTimestamp, currentBlockTimestamp, verify)(blockchain, tx) match {
      case isFailedScript() if acceptFailedScript(blockchain, tx) => failedScriptTransactionDiff(blockchain, tx)
      case result                                                 => result
    }

  def validate(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    for {
      _            <- if (verify) common(prevBlockTimestamp, currentBlockTimestamp)(blockchain, tx) else TracedResult(Right(()))
      diff         <- transactionDiff(currentBlockTimestamp)(blockchain, tx)
      positiveDiff <- balance(blockchain, tx, diff)
      _            <- if (verify) Verifier.assets(blockchain)(tx) else TracedResult(Right(()))
    } yield positiveDiff
  }.leftMap(TransactionValidationError(_, tx))

  private def common(
      prevBlockTimestamp: Option[Long],
      currentBlockTimestamp: Long
  )(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Unit] =
    stats.commonValidation
      .measureForType(tx.typeId) {
        for {
          _ <- TracedResult(
            stats.commonValidation
              .measureForType(tx.typeId) {
                for {
                  _ <- CommonValidation.disallowFromAnotherNetwork(tx, AddressScheme.current.chainId)
                  _ <- CommonValidation.disallowTxFromFuture(blockchain.settings.functionalitySettings, currentBlockTimestamp, tx)
                  _ <- CommonValidation.disallowTxFromPast(blockchain.settings.functionalitySettings, prevBlockTimestamp, tx)
                  _ <- CommonValidation.disallowBeforeActivationTime(blockchain, tx)
                  _ <- CommonValidation.disallowDuplicateIds(blockchain, tx)
                  _ <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, currentBlockTimestamp, tx)
                  _ <- FeeValidation(blockchain, tx)
                } yield ()
              }
          )
          _ <- Verifier(blockchain)(tx)
        } yield ()
      }

  private def transactionDiff(currentBlockTimestamp: Long)(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Diff] =
    stats.transactionDiffValidation.measureForType(tx.typeId) {
      tx match {
        case gtx: GenesisTransaction     => GenesisTransactionDiff(blockchain.height)(gtx)
        case ptx: PaymentTransaction     => PaymentTransactionDiff(blockchain)(ptx)
        case ci: InvokeScriptTransaction => InvokeScriptTransactionDiff(blockchain, currentBlockTimestamp)(ci)
        case etx: ExchangeTransaction    => ExchangeTransactionDiff(blockchain)(etx)
        case otherTx: ProvenTransaction  => estimateDiff(currentBlockTimestamp)(blockchain, otherTx).map(complexityDiff(blockchain, otherTx) |+| _)
        case _                           => Left(UnsupportedTransactionType)
      }
    }

  private def complexityDiff(blockchain: Blockchain, tx: ProvenTransaction): Diff =
    Diff.empty.copy(scriptsComplexity = DiffsCommon.getScriptsComplexity(blockchain, tx))

  private def estimateDiff(
      currentBlockTimestamp: Long
  )(blockchain: Blockchain, tx: ProvenTransaction): TracedResult[ValidationError, Diff] =
    tx match {
      case itx: IssueTransaction             => AssetTransactionsDiff.issue(blockchain)(itx)
      case rtx: ReissueTransaction           => AssetTransactionsDiff.reissue(blockchain, currentBlockTimestamp)(rtx)
      case btx: BurnTransaction              => AssetTransactionsDiff.burn(blockchain)(btx)
      case uaitx: UpdateAssetInfoTransaction => AssetTransactionsDiff.updateInfo(blockchain)(uaitx)
      case ttx: TransferTransaction          => TransferTransactionDiff(blockchain, currentBlockTimestamp)(ttx)
      case mtx: MassTransferTransaction      => MassTransferTransactionDiff(blockchain, currentBlockTimestamp)(mtx)
      case ltx: LeaseTransaction             => LeaseTransactionsDiff.lease(blockchain)(ltx)
      case ltx: LeaseCancelTransaction       => LeaseTransactionsDiff.leaseCancel(blockchain, currentBlockTimestamp)(ltx)
      case atx: CreateAliasTransaction       => CreateAliasTransactionDiff(blockchain)(atx)
      case dtx: DataTransaction              => DataTransactionDiff(blockchain)(dtx)
      case sstx: SetScriptTransaction        => SetScriptTransactionDiff(blockchain)(sstx)
      case sstx: SetAssetScriptTransaction   => AssetTransactionsDiff.setAssetScript(blockchain, currentBlockTimestamp)(sstx)
      case stx: SponsorFeeTransaction        => AssetTransactionsDiff.sponsor(blockchain, currentBlockTimestamp)(stx)
      case _                                 => Left(UnsupportedTransactionType)
    }

  private def balance(blockchain: Blockchain, tx: Transaction, diff: Diff): TracedResult[ValidationError, Diff] =
    stats.balanceValidation
      .measureForType(tx.typeId) {
        TracedResult(BalanceDiffValidation(blockchain)(diff))
      }

  private def failedScriptTransactionDiff(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Diff] =
    for {
      portfolios <- (tx, tx.assetFee) match {
        case (tx: ProvenTransaction, (Waves, fee)) => TracedResult(Right(Map(tx.sender.toAddress -> Portfolio(-fee, LeaseBalance.empty, Map.empty))))
        case (tx: ProvenTransaction, (asset @ IssuedAsset(_), fee)) =>
          TracedResult {
            for {
              assetInfo <- blockchain
                .assetDescription(asset)
                .toRight(GenericError(s"Asset $asset does not exist, cannot be used to pay fees"))
              wavesFee <- Either.cond(
                assetInfo.sponsorship > 0,
                Sponsorship.toWaves(fee, assetInfo.sponsorship),
                GenericError(s"Asset $asset is not sponsored, cannot be used to pay fees")
              )
            } yield {
              Map(tx.sender.toAddress          -> Portfolio(0, LeaseBalance.empty, Map(asset         -> -fee))) |+|
                Map(assetInfo.issuer.toAddress -> Portfolio(-wavesFee, LeaseBalance.empty, Map(asset -> fee)))
            }
          }
        case _ => TracedResult(Left(TransactionValidationError(GenericError(s"Can't accept failed script on $tx"), tx)))
      }
      diff <- balance(blockchain, tx, Diff.failed(tx, portfolios))
    } yield diff

  private def acceptFailedScript(blockchain: Blockchain, tx: Transaction): Boolean =
    (tx.typeId == InvokeScriptTransaction.typeId || tx.typeId == ExchangeTransaction.typeId) &&
      blockchain.isFeatureActivated(BlockchainFeatures.AcceptFailedScriptTransaction)

  private object isFailedScript {
    def unapply(result: TracedResult[ValidationError, Diff]): Boolean =
      result match {
        case TracedResult(Left(TransactionValidationError(ScriptExecutionError(_, _, true), _)), _) => true
        case TracedResult(Left(TransactionValidationError(DAppExecutionError(_, _), _)), _)         => true
        case TracedResult(Left(TransactionValidationError(InsufficientInvokeActionFee(_), _)), _)   => true
        case _                                                                                      => false
      }
  }

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError {
    override def toString: String = s"TransactionValidationError(cause = $cause,\ntx = ${Json.prettyPrint(tx.json())})"
  }
}
