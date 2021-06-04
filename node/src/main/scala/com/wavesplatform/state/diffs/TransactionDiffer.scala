package com.wavesplatform.state.diffs

import scala.collection.mutable

import cats.instances.either._
import cats.instances.map._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.semigroup._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.metrics.TxProcessingStats
import com.wavesplatform.metrics.TxProcessingStats.TxTimerExt
import com.wavesplatform.state.{Blockchain, Diff, InvokeScriptResult, LeaseBalance, NewTransactionInfo, Portfolio, Sponsorship}
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction, Verifier}
import com.wavesplatform.transaction.smart.script.trace.{TracedResult, TraceStep}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import play.api.libs.json.Json

object TransactionDiffer {
  def apply(prevBlockTs: Option[Long], currentBlockTs: Long, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] =
    validate(prevBlockTs, currentBlockTs, verify, limitedExecution = false)(blockchain, tx) match {
      case isFailedTransaction((complexity, scriptResult, trace)) if acceptFailed(blockchain) =>
        TracedResult(failedTransactionDiff(blockchain, tx, complexity, scriptResult), trace)
      case result =>
        result
    }

  def forceValidate(prevBlockTs: Option[Long], currentBlockTs: Long)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] =
    validate(prevBlockTs, currentBlockTs, verify = true, limitedExecution = false)(blockchain, tx)

  def limitedExecution(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    validate(prevBlockTimestamp, currentBlockTimestamp, verify = verify, limitedExecution = transactionMayFail(tx) && acceptFailed(blockchain))(
      blockchain,
      tx
    )
  }

  /**
    * Validates transaction.
    * @param limitedExecution skip execution of the DApp and asset scripts
    * @param verify validate common checks, proofs and asset scripts execution. If `skipFailing` is true asset scripts will not be executed
    */
  private def validate(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, verify: Boolean, limitedExecution: Boolean)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    val verifyAssets = verify || (transactionMayFail(tx) && acceptFailed(blockchain))
    val result = for {
      _               <- validateCommon(blockchain, tx, prevBlockTimestamp, currentBlockTimestamp, verify).traced
      _               <- validateFunds(blockchain, tx).traced
      verifierDiff    <- if (verify) verifierDiff(blockchain, tx) else Right(Diff.empty).traced
      transactionDiff <- transactionDiff(blockchain, tx, verifierDiff, currentBlockTimestamp, limitedExecution)
      remainingComplexity = if (limitedExecution) ContractLimits.FailFreeInvokeComplexity - transactionDiff.scriptsComplexity.toInt else Int.MaxValue
      _ <- validateBalance(blockchain, tx.typeId, transactionDiff).traced.leftMap { err =>
        def acceptFailedByBalance() =
          acceptFailed(blockchain) && blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)

        if (transactionDiff.scriptsComplexity > ContractLimits.FailFreeInvokeComplexity && transactionMayFail(tx) && acceptFailedByBalance())
          FailedTransactionError(FailedTransactionError.Cause.DAppExecution, transactionDiff.scriptsComplexity, Nil, Some(err.toString))
        else
          err
      }
      diff <- assetsVerifierDiff(blockchain, tx, verifyAssets, transactionDiff, remainingComplexity)
    } yield diff
    result.leftMap(TransactionValidationError(_, tx))
  }

  // validation related
  private def validateCommon(
      blockchain: Blockchain,
      tx: Transaction,
      prevBlockTs: Option[Long],
      currentBlockTs: Long,
      verify: Boolean
  ): Either[ValidationError, Unit] =
    if (verify)
      stats.commonValidation
        .measureForType(tx.typeId) {
          for {
            _ <- CommonValidation.disallowFromAnotherNetwork(tx, AddressScheme.current.chainId)
            _ <- CommonValidation.disallowTxFromFuture(blockchain.settings.functionalitySettings, currentBlockTs, tx)
            _ <- CommonValidation.disallowTxFromPast(blockchain.settings.functionalitySettings, prevBlockTs, tx)
            _ <- CommonValidation.disallowBeforeActivationTime(blockchain, tx)
            _ <- CommonValidation.disallowDuplicateIds(blockchain, tx)
            _ <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, currentBlockTs, tx)
            _ <- FeeValidation(blockchain, tx)
          } yield ()
        } else Right(())

  private def validateFunds(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Unit] =
    if (skipFundsSufficiency(blockchain, tx)) Right(())
    else
      for {
        _ <- validateFee(blockchain, tx)
        _ <- tx match {
          case etx: ExchangeTransaction =>
            for {
              _ <- validateOrder(blockchain, etx.buyOrder, etx.buyMatcherFee)
              _ <- validateOrder(blockchain, etx.sellOrder, etx.sellMatcherFee)
            } yield ()
          case itx: InvokeScriptTransaction => validatePayments(blockchain, itx)
          case _                            => Right(())
        }
      } yield ()

  private def verifierDiff(blockchain: Blockchain, tx: Transaction) =
    Verifier(blockchain)(tx).map(complexity => Diff.empty.copy(scriptsComplexity = complexity))

  private def assetsVerifierDiff(
      blockchain: Blockchain,
      tx: Transaction,
      verify: Boolean,
      initDiff: Diff,
      remainingComplexity: Int
  ): TracedResult[ValidationError, Diff] = {
    val diff = if (verify) {
      Verifier.assets(blockchain, remainingComplexity)(tx).leftMap {
        case (spentComplexity, ScriptExecutionError(error, log, Some(assetId))) if transactionMayFail(tx) && acceptFailed(blockchain) =>
          FailedTransactionError.assetExecution(error, spentComplexity, log, assetId)
        case (spentComplexity, TransactionNotAllowedByScript(log, Some(assetId))) if transactionMayFail(tx) && acceptFailed(blockchain) =>
          FailedTransactionError.notAllowedByAsset(spentComplexity, log, assetId)
        case (_, ve) => ve
      }
    } else Diff.empty.asRight[ValidationError].traced

    diff.map(Monoid.combine(initDiff, _)).leftMap {
      case fte: FailedTransactionError => fte.addComplexity(initDiff.scriptsComplexity)
      case ve                          => ve
    }
  }

  private def validateBalance(blockchain: Blockchain, txType: TxType, diff: Diff): Either[ValidationError, Unit] =
    stats.balanceValidation.measureForType(txType)(BalanceDiffValidation(blockchain)(diff).as(()))

  private def transactionDiff(blockchain: Blockchain, tx: Transaction, initDiff: Diff, currentBlockTs: TxTimestamp, limitedExecution: Boolean) =
    stats.transactionDiffValidation
      .measureForType(tx.typeId) {
        tx match {
          case gtx: GenesisTransaction           => GenesisTransactionDiff(blockchain.height)(gtx).traced
          case ptx: PaymentTransaction           => PaymentTransactionDiff(blockchain)(ptx).traced
          case ci: InvokeScriptTransaction       => InvokeScriptTransactionDiff(blockchain, currentBlockTs, limitedExecution)(ci)
          case etx: ExchangeTransaction          => ExchangeTransactionDiff(blockchain)(etx).traced
          case itx: IssueTransaction             => AssetTransactionsDiff.issue(blockchain)(itx).traced
          case rtx: ReissueTransaction           => AssetTransactionsDiff.reissue(blockchain, currentBlockTs)(rtx).traced
          case btx: BurnTransaction              => AssetTransactionsDiff.burn(blockchain)(btx).traced
          case uaitx: UpdateAssetInfoTransaction => AssetTransactionsDiff.updateInfo(blockchain)(uaitx).traced
          case ttx: TransferTransaction          => TransferTransactionDiff(blockchain, currentBlockTs)(ttx).traced
          case mtx: MassTransferTransaction      => MassTransferTransactionDiff(blockchain, currentBlockTs)(mtx).traced
          case ltx: LeaseTransaction             => LeaseTransactionsDiff.lease(blockchain)(ltx).traced
          case ltx: LeaseCancelTransaction       => LeaseTransactionsDiff.leaseCancel(blockchain, currentBlockTs)(ltx).traced
          case atx: CreateAliasTransaction       => CreateAliasTransactionDiff(blockchain)(atx).traced
          case dtx: DataTransaction              => DataTransactionDiff(blockchain)(dtx).traced
          case sstx: SetScriptTransaction        => SetScriptTransactionDiff(blockchain)(sstx).traced
          case sstx: SetAssetScriptTransaction   => AssetTransactionsDiff.setAssetScript(blockchain)(sstx).traced
          case stx: SponsorFeeTransaction        => AssetTransactionsDiff.sponsor(blockchain)(stx).traced
          case _                                 => UnsupportedTransactionType.asLeft.traced
        }
      }
      .map(d => initDiff |+| d.bindTransaction(tx))
      .leftMap {
        case fte: FailedTransactionError => fte.addComplexity(initDiff.scriptsComplexity)
        case ve                          => ve
      }

  // insufficient funds related
  private def skipFundsSufficiency(blockchain: Blockchain, tx: Transaction): Boolean =
    tx match {
      case _: LeaseCancelTransaction      => true
      case _ if !acceptFailed(blockchain) => true
      case _                              => false
    }

  private def validateFee(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Unit] =
    for {
      fee <- feePortfolios(blockchain, tx)
      _   <- validateBalance(blockchain, tx.typeId, Diff(portfolios = fee))
    } yield ()

  private def validateOrder(blockchain: Blockchain, order: Order, matcherFee: Long): Either[ValidationError, Unit] =
    for {
      _ <- order.matcherFeeAssetId match {
        case Waves => Right(())
        case asset @ IssuedAsset(_) =>
          blockchain
            .assetDescription(asset)
            .toRight(GenericError(s"Asset $asset should be issued before it can be traded"))
      }
      orderDiff = Diff.empty.copy(portfolios = Map(order.sender.toAddress -> Portfolio.build(order.matcherFeeAssetId, -matcherFee)))
      _ <- validateBalance(blockchain, ExchangeTransaction.typeId, orderDiff)
    } yield ()

  private def validatePayments(blockchain: Blockchain, tx: InvokeScriptTransaction): Either[ValidationError, Unit] =
    for {
      dAppAddress <- blockchain.resolveAlias(tx.dAppAddressOrAlias)
      portfolios <- tx.payments
        .map {
          case InvokeScriptTransaction.Payment(amt, assetId) =>
            assetId match {
              case asset @ IssuedAsset(_) =>
                blockchain
                  .assetDescription(asset)
                  .toRight(GenericError(s"Referenced $asset not found"))
                  .as(
                    Monoid.combine(
                      Map(tx.sender.toAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -amt))),
                      Map(dAppAddress         -> Portfolio(0, LeaseBalance.empty, Map(asset -> amt)))
                    )
                  )
              case Waves =>
                Monoid
                  .combine(
                    Map(tx.sender.toAddress -> Portfolio(-amt, LeaseBalance.empty, Map.empty)),
                    Map(dAppAddress         -> Portfolio(amt, LeaseBalance.empty, Map.empty))
                  )
                  .asRight
            }
        }
        .foldLeft[Either[ValidationError, Map[Address, Portfolio]]](Right(Map.empty))(Monoid.combine)
      paymentsDiff = Diff.empty.copy(portfolios = portfolios)
      _ <- BalanceDiffValidation(blockchain)(paymentsDiff)
    } yield ()

  // failed transactions related
  private def transactionMayFail(tx: Transaction): Boolean = tx.typeId == InvokeScriptTransaction.typeId || tx.typeId == ExchangeTransaction.typeId

  private def acceptFailed(blockchain: Blockchain): Boolean = blockchain.isFeatureActivated(BlockV5)

  private def failedTransactionDiff(
      blockchain: Blockchain,
      tx: Transaction,
      spentComplexity: Long,
      scriptResult: Option[InvokeScriptResult]
  ): Either[ValidationError, Diff] = {
    val extractDAppAddress = tx match {
      case ist: InvokeScriptTransaction => blockchain.resolveAlias(ist.dAppAddressOrAlias).map(Some(_))
      case _                            => Right(None)
    }

    for {
      portfolios <- feePortfolios(blockchain, tx)
      maybeDApp  <- extractDAppAddress
      calledAddresses = scriptResult.map(inv => InvokeScriptResult.Invocation.calledAddresses(inv.invokes)).getOrElse(Nil)
    } yield {
      val affectedAddresses = portfolios.keySet ++ maybeDApp ++ calledAddresses
      Diff.empty.copy(
        transactions = mutable.LinkedHashMap((tx.id(), NewTransactionInfo(tx, affectedAddresses, applied = false))),
        portfolios = portfolios,
        scriptResults = scriptResult.fold(Map.empty[ByteStr, InvokeScriptResult])(sr => Map(tx.id() -> sr)),
        scriptsComplexity = spentComplexity
      )
    }
  }

  private object isFailedTransaction {
    def unapply(result: TracedResult[ValidationError, Diff]): Option[(Long, Option[InvokeScriptResult], List[TraceStep])] =
      result match {
        case TracedResult(Left(TransactionValidationError(e: FailedTransactionError, _)), trace) => Some((e.spentComplexity, scriptResult(e), trace))
        case _                                                                                   => None
      }

    private[this] def scriptResult(cf: FailedTransactionError): Option[InvokeScriptResult] =
      Some(InvokeScriptResult(error = Some(ErrorMessage(cf.code, cf.message)), invokes = cf.invocations))
  }

  // helpers
  private def feePortfolios(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Map[Address, Portfolio]] =
    tx match {
      case _: GenesisTransaction   => Map.empty[Address, Portfolio].asRight
      case ptx: PaymentTransaction => Map(ptx.sender.toAddress -> Portfolio(balance = -ptx.fee, LeaseBalance.empty, assets = Map.empty)).asRight
      case ptx: ProvenTransaction =>
        ptx.assetFee match {
          case (Waves, fee) => Map(ptx.sender.toAddress -> Portfolio(-fee, LeaseBalance.empty, Map.empty)).asRight
          case (asset @ IssuedAsset(_), fee) =>
            for {
              assetInfo <- blockchain
                .assetDescription(asset)
                .toRight(GenericError(s"Asset $asset does not exist, cannot be used to pay fees"))
              wavesFee <- Either.cond(
                assetInfo.sponsorship > 0,
                Sponsorship.toWaves(fee, assetInfo.sponsorship),
                GenericError(s"Asset $asset is not sponsored, cannot be used to pay fees")
              )
            } yield Monoid.combine(
              Map(ptx.sender.toAddress       -> Portfolio(0, LeaseBalance.empty, Map(asset         -> -fee))),
              Map(assetInfo.issuer.toAddress -> Portfolio(-wavesFee, LeaseBalance.empty, Map(asset -> fee)))
            )
        }
      case _ => UnsupportedTransactionType.asLeft
    }

  private implicit final class EitherOps[E, A](val ei: Either[E, A]) extends AnyVal {
    // Not really traced, just wraps value with an empty trace value
    def traced: TracedResult[E, A] = TracedResult.wrapE(ei)
  }

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError {
    override def toString: String = s"TransactionValidationError(cause = $cause,\ntx = ${Json.prettyPrint(tx.json())})"
  }

  private val stats = TxProcessingStats
}
