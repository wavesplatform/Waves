package com.wavesplatform.state.diffs

import cats.implicits.{toFoldableOps, toTraverseOps}
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
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.{Blockchain, Diff, InvokeScriptResult, NewTransactionInfo, Portfolio, Sponsorship}
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
  def apply(prevBlockTs: Option[Long], currentBlockTs: Long, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] =
    validate(prevBlockTs, currentBlockTs, verify, limitedExecution = false)(blockchain, tx) match {
      case isFailedTransaction((complexity, scriptResult, trace, attributes)) if acceptFailed(blockchain) =>
        TracedResult(failedTransactionDiff(blockchain, tx, complexity, scriptResult), trace, attributes)
      case result =>
        result
    }

  def forceValidate(prevBlockTs: Option[Long], currentBlockTs: Long)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] =
    validate(prevBlockTs, currentBlockTs, verify = true, limitedExecution = false)(blockchain, tx)

  def limitedExecution(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, unlimited: Boolean, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    val limitedExecution = if (unlimited) false else transactionMayFail(tx) && acceptFailed(blockchain)
    validate(prevBlockTimestamp, currentBlockTimestamp, verify = verify, limitedExecution)(
      blockchain,
      tx
    )
  }

  /** Validates transaction.
    * @param limitedExecution
    *   skip execution of the DApp and asset scripts
    * @param verify
    *   validate common checks, proofs and asset scripts execution. If `skipFailing` is true asset scripts will not be executed
    */
  private def validate(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, verify: Boolean, limitedExecution: Boolean)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    val runVerifiers = verify || (transactionMayFail(tx) && acceptFailed(blockchain))
    val result = for {
      _               <- validateCommon(blockchain, tx, prevBlockTimestamp, currentBlockTimestamp, verify).traced
      _               <- validateFunds(blockchain, tx).traced
      verifierDiff    <- if (runVerifiers) verifierDiff(blockchain, tx) else Right(Diff.empty).traced
      transactionDiff <- transactionDiff(blockchain, tx, verifierDiff, currentBlockTimestamp, limitedExecution)
      remainingComplexity = if (limitedExecution) ContractLimits.FailFreeInvokeComplexity - transactionDiff.scriptsComplexity.toInt else Int.MaxValue
      _ <- validateBalance(blockchain, tx.tpe, transactionDiff).traced.leftMap { err =>
        def acceptFailedByBalance(): Boolean =
          acceptFailed(blockchain) && blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)

        if (transactionDiff.scriptsComplexity > ContractLimits.FailFreeInvokeComplexity && transactionMayFail(tx) && acceptFailedByBalance())
          FailedTransactionError(FailedTransactionError.Cause.DAppExecution, transactionDiff.scriptsComplexity, Nil, Some(err.toString))
        else
          err
      }
      diff <- assetsVerifierDiff(blockchain, tx, runVerifiers, transactionDiff, remainingComplexity)
    } yield diff

    result
      .leftMap {
        // Force reject
        case fte: FailedTransactionError if fte.isFailFree && blockchain.isFeatureActivated(RideV6) && fte.isDAppExecution =>
          InvokeRejectError(fte.message, fte.log)
        case fte: FailedTransactionError if fte.isFailFree && blockchain.isFeatureActivated(RideV6) =>
          ScriptExecutionError(fte.message, fte.log, fte.assetId)
        case err => err
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
                if (blockchain.height >= blockchain.settings.functionalitySettings.estimatorSumOverflowFixHeight) {
                  ExchangeTransactionDiff
                    .getPortfolios(blockchain, etx)
                    .flatMap(pfs => validateBalance(blockchain, etx.tpe, Diff(portfolios = pfs)))
                } else Right(())
            } yield ()
          case itx: InvokeScriptTransaction => validatePayments(blockchain, itx)
          case _                            => Right(())
        }
      } yield ()

  private[this] def verifierDiff(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Diff] =
    Verifier(blockchain)(tx).map(complexity => Diff(scriptsComplexity = complexity))

  def assetsVerifierDiff(
      blockchain: Blockchain,
      tx: TransactionBase,
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

    diff.flatMap(d => initDiff.combineE(d)).leftMap {
      case fte: FailedTransactionError => fte.addComplexity(initDiff.scriptsComplexity)
      case ve                          => ve
    }
  }

  private def validateBalance(blockchain: Blockchain, txType: Transaction.Type, diff: Diff): Either[ValidationError, Unit] =
    stats.balanceValidation.measureForType(txType)(BalanceDiffValidation(blockchain)(diff).as(()))

  private def transactionDiff(
      blockchain: Blockchain,
      tx: Transaction,
      initDiff: Diff,
      currentBlockTs: TxTimestamp,
      limitedExecution: Boolean
  ): TracedResult[ValidationError, Diff] =
    stats.transactionDiffValidation
      .measureForType(tx.tpe) {
        tx match {
          case gtx: GenesisTransaction           => GenesisTransactionDiff(blockchain.height)(gtx).traced
          case ptx: PaymentTransaction           => PaymentTransactionDiff(blockchain)(ptx).traced
          case ci: InvokeTransaction             => InvokeScriptTransactionDiff(blockchain, currentBlockTs, limitedExecution)(ci)
          case etx: ExchangeTransaction          => ExchangeTransactionDiff(blockchain)(etx).traced
          case itx: IssueTransaction             => AssetTransactionsDiff.issue(blockchain)(itx).traced
          case rtx: ReissueTransaction           => AssetTransactionsDiff.reissue(blockchain, currentBlockTs)(rtx).traced
          case btx: BurnTransaction              => AssetTransactionsDiff.burn(blockchain)(btx).traced
          case uaitx: UpdateAssetInfoTransaction => AssetTransactionsDiff.updateInfo(blockchain)(uaitx).traced
          case ttx: TransferTransaction          => TransferTransactionDiff(blockchain)(ttx).traced
          case mtx: MassTransferTransaction      => MassTransferTransactionDiff(blockchain, currentBlockTs)(mtx).traced
          case ltx: LeaseTransaction             => LeaseTransactionsDiff.lease(blockchain)(ltx).traced
          case ltx: LeaseCancelTransaction       => LeaseTransactionsDiff.leaseCancel(blockchain, currentBlockTs)(ltx).traced
          case atx: CreateAliasTransaction       => CreateAliasTransactionDiff(blockchain)(atx).traced
          case dtx: DataTransaction              => DataTransactionDiff(blockchain)(dtx).traced
          case sstx: SetScriptTransaction        => SetScriptTransactionDiff(blockchain)(sstx).traced
          case sstx: SetAssetScriptTransaction   => AssetTransactionsDiff.setAssetScript(blockchain)(sstx).traced
          case stx: SponsorFeeTransaction        => AssetTransactionsDiff.sponsor(blockchain)(stx).traced
          case et: EthereumTransaction           => EthereumTransactionDiff(blockchain, currentBlockTs, limitedExecution)(et)
          case _                                 => UnsupportedTransactionType.asLeft.traced
        }
      }
      .flatMap(diff => initDiff.combineE(diff.bindTransaction(blockchain, tx, applied = true)))
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
      _   <- validateBalance(blockchain, tx.tpe, Diff(portfolios = fee))
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
      orderDiff = Diff(portfolios = Map(order.sender.toAddress -> Portfolio.build(order.matcherFeeAssetId, -matcherFee)))
      _ <- validateBalance(blockchain, TransactionType.Exchange, orderDiff)
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
                  Diff
                    .combine(
                      Map[Address, Portfolio](tx.senderAddress -> Portfolio.build(asset -> -amt)),
                      Map[Address, Portfolio](dAppAddress      -> Portfolio.build(asset -> amt))
                    )
                    .leftMap(GenericError(_))
                )
            case Waves =>
              Diff
                .combine(
                  Map[Address, Portfolio](tx.senderAddress -> Portfolio(-amt)),
                  Map[Address, Portfolio](dAppAddress      -> Portfolio(amt))
                )
                .leftMap(GenericError(_))
          }
        }
        .flatMap(_.foldM(Map.empty[Address, Portfolio])(Diff.combine).leftMap(GenericError(_)))
      paymentsDiff = Diff(portfolios = portfolios)
      _ <- BalanceDiffValidation(blockchain)(paymentsDiff)
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
  ): Either[ValidationError, Diff] = {
    val extractDAppAddress = tx match {
      case it: InvokeTransaction => blockchain.resolveAlias(it.dApp).map(Some(_))
      case _                     => Right(None)
    }

    for {
      portfolios <- feePortfolios(blockchain, tx)
      maybeDApp  <- extractDAppAddress
      calledAddresses = scriptResult.map(inv => InvokeScriptResult.Invocation.calledAddresses(inv.invokes)).getOrElse(Nil)
    } yield {
      val affectedAddresses = portfolios.keySet ++ maybeDApp ++ calledAddresses
      val ethereumMetaDiff = tx match {
        case e: EthereumTransaction => EthereumTransactionDiff.meta(blockchain)(e)
        case _                      => Diff.empty
      }
      Diff.withTransactions(
        Vector(NewTransactionInfo(tx, affectedAddresses, applied = false, spentComplexity)),
        portfolios = portfolios,
        scriptResults = scriptResult.fold(Map.empty[ByteStr, InvokeScriptResult])(sr => Map(tx.id() -> sr)),
        scriptsComplexity = spentComplexity
      ).combineF(ethereumMetaDiff).getOrElse(Diff.empty)
    }
  }

  private object isFailedTransaction {
    def unapply(result: TracedResult[ValidationError, Diff]): Option[(Long, Option[InvokeScriptResult], List[TraceStep], TracedResult.Attributes)] =
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
              portfolios <- Diff
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
