package com.wavesplatform.state.diffs

import cats.implicits.{catsSyntaxSemigroup, toFoldableOps, toTraverseOps}
import cats.instances.either.*
import cats.syntax.either.*
import cats.syntax.functor.*
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.{BlockV5, RideV6}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.metrics.TxProcessingStats
import com.wavesplatform.metrics.TxProcessingStats.TxTimerExt
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.{Blockchain, InvokeScriptResult, NewTransactionInfo, Portfolio, Sponsorship, StateSnapshot}
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.*
import com.wavesplatform.transaction.smart.script.trace.{TraceStep, TracedResult}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import play.api.libs.json.Json

object TransactionDiffer {
  def apply(prevBlockTs: Option[Long], currentBlockTs: Long, verify: Boolean = true, enableExecutionLog: Boolean = false)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, StateSnapshot] =
    validate(prevBlockTs, currentBlockTs, verify, limitedExecution = false, enableExecutionLog = enableExecutionLog)(blockchain, tx) match {
      case isFailedTransaction((complexity, scriptResult, trace, attributes)) if acceptFailed(blockchain) =>
        TracedResult(failedTransactionDiff(blockchain, tx, complexity, scriptResult), trace, attributes)
      case result =>
        result
    }

  def forceValidate(prevBlockTs: Option[Long], currentBlockTs: Long, enableExecutionLog: Boolean = false)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, StateSnapshot] =
    validate(prevBlockTs, currentBlockTs, verify = true, limitedExecution = false, enableExecutionLog = enableExecutionLog)(blockchain, tx)

  def limitedExecution(
      prevBlockTimestamp: Option[Long],
      currentBlockTimestamp: Long,
      unlimited: Boolean,
      verify: Boolean = true,
      enableExecutionLog: Boolean = false
  )(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, StateSnapshot] = {
    val limitedExecution = if (unlimited) false else transactionMayFail(tx) && acceptFailed(blockchain)
    validate(
      prevBlockTimestamp,
      currentBlockTimestamp,
      verify = verify,
      limitedExecution = limitedExecution,
      enableExecutionLog = enableExecutionLog
    )(blockchain, tx)
  }

  /** Validates transaction.
    * @param limitedExecution
    *   skip execution of the DApp and asset scripts
    * @param verify
    *   validate common checks, proofs and asset scripts execution. If `skipFailing` is true asset scripts will not be executed
    */
  private def validate(
      prevBlockTimestamp: Option[Long],
      currentBlockTimestamp: Long,
      verify: Boolean,
      limitedExecution: Boolean,
      enableExecutionLog: Boolean
  )(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, StateSnapshot] = {
    val runVerifiers = verify || (transactionMayFail(tx) && acceptFailed(blockchain))
    val result = for {
      _                   <- validateCommon(blockchain, tx, prevBlockTimestamp, currentBlockTimestamp, verify).traced
      _                   <- validateFunds(blockchain, tx).traced
      verifierSnapshot    <- if (runVerifiers) verifierDiff(blockchain, tx, enableExecutionLog) else TracedResult.wrapValue(StateSnapshot.empty)
      transactionSnapshot <- transactionSnapshot(blockchain, tx, verifierSnapshot, currentBlockTimestamp, limitedExecution, enableExecutionLog)
      remainingComplexity =
        if (limitedExecution) ContractLimits.FailFreeInvokeComplexity - transactionSnapshot.scriptsComplexity.toInt else Int.MaxValue
      _ <- validateBalance(blockchain, tx.tpe, transactionSnapshot).traced.leftMap { err =>
        def acceptFailedByBalance(): Boolean =
          acceptFailed(blockchain) && blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)

        if (transactionSnapshot.scriptsComplexity > ContractLimits.FailFreeInvokeComplexity && transactionMayFail(tx) && acceptFailedByBalance())
          FailedTransactionError(FailedTransactionError.Cause.DAppExecution, transactionSnapshot.scriptsComplexity, Nil, Some(err.toString))
        else
          err
      }
      snapshot <- assetsVerifierDiff(blockchain, tx, runVerifiers, transactionSnapshot, remainingComplexity, enableExecutionLog)
    } yield snapshot

    result
      .leftMap {
        // Force reject
        case fte: FailedTransactionError if fte.isFailFree && blockchain.isFeatureActivated(RideV6) && fte.isDAppExecution =>
          InvokeRejectError(fte.message, fte.log)
        case fte: FailedTransactionError if fte.isFailFree && blockchain.isFeatureActivated(RideV6) =>
          ScriptExecutionError(fte.message, fte.log, fte.assetId)
        case err =>
          err
      }
      .leftMap(TransactionValidationError(_, tx))
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
        .measureForType(tx.tpe) {
          for {
            _ <- CommonValidation.disallowFromAnotherNetwork(tx, AddressScheme.current.chainId)
            _ <- CommonValidation.disallowTxFromFuture(blockchain.settings.functionalitySettings, currentBlockTs, tx)
            _ <- CommonValidation.disallowTxFromPast(blockchain.settings.functionalitySettings, prevBlockTs, tx)
            _ <- CommonValidation.disallowBeforeActivationTime(blockchain, tx)
            _ <- CommonValidation.disallowDuplicateIds(blockchain, tx)
            _ <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, currentBlockTs, tx)
            _ <- FeeValidation(blockchain, tx)
          } yield ()
        }
    else Right(())

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

              // Balance overflow check
              _ <-
                if (blockchain.height >= blockchain.settings.functionalitySettings.estimatorSumOverflowFixHeight)
                  for {
                    portfolios <- ExchangeTransactionDiff.getPortfolios(blockchain, etx)
                    snapshot   <- StateSnapshot.build(blockchain, portfolios)
                    _          <- validateBalance(blockchain, etx.tpe, snapshot)
                  } yield portfolios
                else
                  Right(())
            } yield ()
          case itx: InvokeScriptTransaction => validatePayments(blockchain, itx)
          case _                            => Right(())
        }
      } yield ()

  private[this] def verifierDiff(blockchain: Blockchain, tx: Transaction, enableExecutionLog: Boolean): TracedResult[ValidationError, StateSnapshot] =
    Verifier(blockchain, enableExecutionLog = enableExecutionLog)(tx)
      .map(complexity => StateSnapshot(scriptsComplexity = complexity))

  def assetsVerifierDiff(
      blockchain: Blockchain,
      tx: TransactionBase,
      verify: Boolean,
      initSnapshot: StateSnapshot,
      remainingComplexity: Int,
      enableExecutionLog: Boolean
  ): TracedResult[ValidationError, StateSnapshot] = {
    val snapshot = if (verify) {
      Verifier.assets(blockchain, remainingComplexity, enableExecutionLog)(tx).leftMap {
        case (spentComplexity, ScriptExecutionError(error, log, Some(assetId))) if transactionMayFail(tx) && acceptFailed(blockchain) =>
          FailedTransactionError.assetExecution(error, spentComplexity, log, assetId)
        case (spentComplexity, TransactionNotAllowedByScript(log, Some(assetId))) if transactionMayFail(tx) && acceptFailed(blockchain) =>
          FailedTransactionError.notAllowedByAsset(spentComplexity, log, assetId)
        case (_, ve) => ve
      }
    } else StateSnapshot.empty.asRight[ValidationError].traced

    snapshot
      .map(initSnapshot |+| _)
      .leftMap {
        case fte: FailedTransactionError => fte.addComplexity(initSnapshot.scriptsComplexity)
        case ve                          => ve
      }
  }

  def validateBalance(blockchain: Blockchain, txType: Transaction.Type, s: StateSnapshot): Either[ValidationError, Unit] =
    stats.balanceValidation.measureForType(txType)(BalanceDiffValidation(blockchain)(s).as(()))

  private def transactionSnapshot(
      blockchain: Blockchain,
      tx: Transaction,
      initSnapshot: StateSnapshot,
      currentBlockTs: TxTimestamp,
      limitedExecution: Boolean,
      enableExecutionLog: Boolean
  ): TracedResult[ValidationError, StateSnapshot] =
    stats.transactionDiffValidation
      .measureForType(tx.tpe) {
        tx match {
          case gtx: GenesisTransaction           => GenesisTransactionDiff(blockchain)(gtx).traced
          case ptx: PaymentTransaction           => PaymentTransactionDiff(blockchain)(ptx).traced
          case ci: InvokeTransaction             => InvokeScriptTransactionDiff(blockchain, currentBlockTs, limitedExecution, enableExecutionLog)(ci)
          case etx: ExchangeTransaction          => ExchangeTransactionDiff(blockchain)(etx).traced
          case itx: IssueTransaction             => AssetTransactionsDiffs.issue(blockchain)(itx).traced
          case rtx: ReissueTransaction           => AssetTransactionsDiffs.reissue(blockchain, currentBlockTs)(rtx).traced
          case btx: BurnTransaction              => AssetTransactionsDiffs.burn(blockchain)(btx).traced
          case uaitx: UpdateAssetInfoTransaction => AssetTransactionsDiffs.updateInfo(blockchain)(uaitx).traced
          case ttx: TransferTransaction          => TransferTransactionDiff(blockchain)(ttx).traced
          case mtx: MassTransferTransaction      => MassTransferTransactionDiff(blockchain)(mtx).traced
          case ltx: LeaseTransaction             => LeaseTransactionsDiff.lease(blockchain)(ltx).traced
          case ltx: LeaseCancelTransaction       => LeaseTransactionsDiff.leaseCancel(blockchain, currentBlockTs)(ltx).traced
          case atx: CreateAliasTransaction       => CreateAliasTransactionDiff(blockchain)(atx).traced
          case dtx: DataTransaction              => DataTransactionDiff(blockchain)(dtx).traced
          case sstx: SetScriptTransaction        => SetScriptTransactionDiff(blockchain)(sstx).traced
          case sstx: SetAssetScriptTransaction   => AssetTransactionsDiffs.setAssetScript(blockchain)(sstx).traced
          case stx: SponsorFeeTransaction        => AssetTransactionsDiffs.sponsor(blockchain)(stx).traced
          case et: EthereumTransaction           => EthereumTransactionDiff(blockchain, currentBlockTs, limitedExecution, enableExecutionLog)(et)
          case _                                 => UnsupportedTransactionType.asLeft.traced
        }
      }
      .map(txSnapshot => initSnapshot |+| txSnapshot.withTransaction(NewTransactionInfo.create(tx, Status.Succeeded, txSnapshot, blockchain)))
      .leftMap {
        case fte: FailedTransactionError => fte.addComplexity(initSnapshot.scriptsComplexity)
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
      fee      <- feePortfolios(blockchain, tx)
      snapshot <- StateSnapshot.build(blockchain, fee)
      _        <- validateBalance(blockchain, tx.tpe, snapshot)
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
      portfolios = Map(order.sender.toAddress -> Portfolio.build(order.matcherFeeAssetId, -matcherFee))
      snapshot <- StateSnapshot.build(blockchain, portfolios)
      _        <- validateBalance(blockchain, TransactionType.Exchange, snapshot)
    } yield ()

  private def validatePayments(blockchain: Blockchain, tx: InvokeScriptTransaction): Either[ValidationError, Unit] =
    for {
      dAppAddress <- blockchain.resolveAlias(tx.dApp)
      portfolios <- tx.payments
        .traverse { case InvokeScriptTransaction.Payment(amt, assetId) =>
          assetId match {
            case asset @ IssuedAsset(_) =>
              blockchain
                .assetDescription(asset)
                .toRight(GenericError(s"Referenced $asset not found"))
                .flatMap(_ =>
                  Portfolio
                    .combine(
                      Map[Address, Portfolio](tx.senderAddress -> Portfolio.build(asset -> -amt)),
                      Map[Address, Portfolio](dAppAddress      -> Portfolio.build(asset -> amt))
                    )
                    .leftMap(GenericError(_))
                )
            case Waves =>
              Portfolio
                .combine(
                  Map[Address, Portfolio](tx.senderAddress -> Portfolio(-amt)),
                  Map[Address, Portfolio](dAppAddress      -> Portfolio(amt))
                )
                .leftMap(GenericError(_))
          }
        }
        .flatMap(_.foldM(Map.empty[Address, Portfolio])(Portfolio.combine).leftMap(GenericError(_)))
      paymentsSnapshot <- StateSnapshot.build(blockchain, portfolios)
      _                <- BalanceDiffValidation(blockchain)(paymentsSnapshot)
    } yield ()

  // failed transactions related
  private def transactionMayFail(tx: TransactionBase): Boolean =
    tx.tpe == TransactionType.InvokeScript ||
      tx.tpe == TransactionType.InvokeExpression ||
      tx.tpe == TransactionType.Exchange

  private def acceptFailed(blockchain: Blockchain): Boolean = blockchain.isFeatureActivated(BlockV5)

  private def failedTransactionDiff(
      blockchain: Blockchain,
      tx: Transaction,
      spentComplexity: Long,
      scriptResult: Option[InvokeScriptResult]
  ): Either[ValidationError, StateSnapshot] =
    for {
      portfolios <- feePortfolios(blockchain, tx)
      ethereumMetaDiff = tx match {
        case e: EthereumTransaction => EthereumTransactionDiff.meta(blockchain)(e)
        case _                      => StateSnapshot.empty
      }
      snapshot <- StateSnapshot
        .build(
          blockchain,
          portfolios = portfolios,
          scriptResults = scriptResult.fold(Map.empty[ByteStr, InvokeScriptResult])(sr => Map(tx.id() -> sr)),
          scriptsComplexity = spentComplexity
        )
        .map(_ |+| ethereumMetaDiff)
    } yield snapshot.withTransaction(NewTransactionInfo.create(tx, Status.Failed, snapshot, blockchain))

  private object isFailedTransaction {
    def unapply(
        result: TracedResult[ValidationError, StateSnapshot]
    ): Option[(Long, Option[InvokeScriptResult], List[TraceStep], TracedResult.Attributes)] =
      result match {
        case TracedResult(Left(TransactionValidationError(e: FailedTransactionError, _)), trace, attributes) =>
          Some((e.spentComplexity, scriptResult(e), trace, attributes))
        case _ => None
      }

    private[this] def scriptResult(cf: FailedTransactionError): Option[InvokeScriptResult] =
      Some(InvokeScriptResult(error = Some(ErrorMessage(cf.code, cf.message)), invokes = cf.invocations))
  }

  // helpers
  private def feePortfolios(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Map[Address, Portfolio]] =
    tx match {
      case _: GenesisTransaction => Map.empty[Address, Portfolio].asRight
      case ptx: PaymentTransaction =>
        Map[Address, Portfolio](ptx.sender.toAddress -> Portfolio(balance = -ptx.fee.value)).asRight
      case e: EthereumTransaction => Map[Address, Portfolio](e.senderAddress() -> Portfolio(-e.fee)).asRight
      case ptx: ProvenTransaction =>
        ptx.assetFee match {
          case (Waves, fee) => Map[Address, Portfolio](ptx.sender.toAddress -> Portfolio(-fee)).asRight
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
              portfolios <- Portfolio
                .combine(
                  Map(ptx.sender.toAddress       -> Portfolio.build(asset, -fee)),
                  Map(assetInfo.issuer.toAddress -> Portfolio.build(-wavesFee, asset, fee))
                )
                .leftMap(GenericError(_))
            } yield portfolios
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
