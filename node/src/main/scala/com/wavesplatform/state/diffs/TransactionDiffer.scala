package com.wavesplatform.state.diffs

import cats.instances.either._
import cats.instances.map._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.functor._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.TxProcessingStats
import com.wavesplatform.metrics.TxProcessingStats.TxTimerExt
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.{Blockchain, Diff, InvokeScriptResult, LeaseBalance, NewTransactionInfo, Portfolio, Sponsorship}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction, Verifier}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import play.api.libs.json.Json

import scala.collection.mutable

object TransactionDiffer {
  def apply(prevBlockTs: Option[Long], currentBlockTs: Long, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] =
    validate(prevBlockTs, currentBlockTs, verify, skipFailing = false)(blockchain, tx) match {
      case isFailedTransaction((complexity, scriptResult)) if acceptFailed(blockchain) =>
        failedTransactionDiff(blockchain, tx, complexity, scriptResult).traced
      case result => result
    }

  /** Validates transaction but since BlockV5 skips ability to fail (does not execute asset scripts and dApp script for ExchangeTx and InvokeScriptTx) */
  def skipFailing(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, verify: Boolean = true)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    validate(prevBlockTimestamp, currentBlockTimestamp, verify = verify, skipFailing = mayFail(tx) && acceptFailed(blockchain))(blockchain, tx)
  }

  /**
    * Validates transaction.
    * @param skipFailing skip execution of the DApp and asset scripts
    * @param verify validate common checks, proofs and asset scripts execution. If `skipFailing` is true asset scripts will not be executed
    */
  private def validate(prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, verify: Boolean, skipFailing: Boolean)(
      blockchain: Blockchain,
      tx: Transaction
  ): TracedResult[ValidationError, Diff] = {
    // do not validate assets if `skipFailing` is `true` because assets verification is the only way (other than DApp execution) to fail the transaction
    // otherwise validate assets if `verify` is `true` or the transaction may fail
    val verifyAssets = if (skipFailing) false else verify || (mayFail(tx) && acceptFailed(blockchain))
    val result = for {
      _               <- validateCommon(blockchain, tx, prevBlockTimestamp, currentBlockTimestamp, verify).traced
      _               <- validateFunds(blockchain, tx).traced
      verifierDiff    <- verifierDiff(blockchain, tx, verify)
      transactionDiff <- transactionDiff(blockchain, tx, verifierDiff, currentBlockTimestamp, skipFailing, verifyAssets)
      _               <- validateBalance(blockchain, tx.typeId, transactionDiff).traced
      diff            <- assetsVerifierDiff(blockchain, tx, verifyAssets, transactionDiff)
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

  private def verifierDiff(blockchain: Blockchain, tx: Transaction, verify: Boolean): TracedResult[ValidationError, Diff] =
    if (verify) {
      Verifier(blockchain)(tx) match {
        case (complexity, ttx) => ttx.as(Diff.empty.copy(scriptsComplexity = DiffsCommon.getAccountsComplexity(blockchain, tx), spentComplexity = complexity))
      }
    } else Right(Diff.empty).traced

  private def assetsVerifierDiff(blockchain: Blockchain, tx: Transaction, verify: Boolean, initDiff: Diff): TracedResult[ValidationError, Diff] = {
    val diff = if (verify) {
      Verifier.assets(blockchain)(tx).leftMap {
        case (spentComplexity, real, ScriptExecutionError(error, log, Some(assetId))) if mayFail(tx) && acceptFailed(blockchain) =>
          FailedTransactionError.assetExecution(error, spentComplexity, real, log, assetId)
        case (spentComplexity, real, TransactionNotAllowedByScript(log, Some(assetId))) if mayFail(tx) && acceptFailed(blockchain) =>
          FailedTransactionError.notAllowedByAsset(spentComplexity, real, log, assetId)
        case (_, _, ve) => ve
      }
    } else Diff.empty.asRight[ValidationError].traced

    diff.map(Monoid.combine(initDiff, _)).leftMap {
      case fte: FailedTransactionError => fte.addComplexity(initDiff.scriptsComplexity, initDiff.spentComplexity)
      case ve                          => ve
    }
  }

  private def validateBalance(blockchain: Blockchain, txType: TxType, diff: Diff): Either[ValidationError, Unit] =
    stats.balanceValidation.measureForType(txType)(BalanceDiffValidation(blockchain)(diff).as(()))

  private def transactionDiff(
      blockchain: Blockchain,
      tx: Transaction,
      initDiff: Diff,
      currentBlockTs: Long,
      skipFailing: Boolean,
      verifyAssets: Boolean
  ): TracedResult[ValidationError, Diff] = {
    val diff = stats.transactionDiffValidation.measureForType(tx.typeId) {
      tx match {
        case gtx: GenesisTransaction           => GenesisTransactionDiff(blockchain.height)(gtx).traced
        case ptx: PaymentTransaction           => PaymentTransactionDiff(blockchain)(ptx).traced
        case ci: InvokeScriptTransaction       => InvokeScriptTransactionDiff(blockchain, currentBlockTs, skipFailing, verifyAssets)(ci)
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
        case sstx: SetAssetScriptTransaction   => AssetTransactionsDiff.setAssetScript(blockchain, currentBlockTs)(sstx).traced
        case stx: SponsorFeeTransaction        => AssetTransactionsDiff.sponsor(blockchain)(stx).traced
        case _                                 => UnsupportedTransactionType.asLeft.traced
      }
    }

    diff
      .map(d => Monoid.combine(initDiff, d))
      .leftMap {
        case fte: FailedTransactionError => fte.addComplexity(initDiff.scriptsComplexity, initDiff.spentComplexity)
        case ve                          => ve
      }
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
      _   <- validateBalance(blockchain, tx.typeId, Diff(tx, portfolios = fee))
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
  private def mayFail(tx: Transaction): Boolean = tx.typeId == InvokeScriptTransaction.typeId || tx.typeId == ExchangeTransaction.typeId

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
    } yield {
      Diff.empty.copy(
        transactions = mutable.LinkedHashMap((tx.id(), NewTransactionInfo(tx, (portfolios.keys ++ maybeDApp.toList).toSet, applied = false))),
        portfolios = portfolios,
        scriptResults = scriptResult.fold(Map.empty[ByteStr, InvokeScriptResult])(sr => Map(tx.id() -> sr)),
        scriptsComplexity = spentComplexity
      )
    }
  }

  private object isFailedTransaction {
    def unapply(result: TracedResult[ValidationError, Diff]): Option[(Long, Option[InvokeScriptResult])] =
      result match {
        case TracedResult(Left(TransactionValidationError(e: FailedTransactionError, tx)), _) => Some((e.spentComplexity, scriptResult(e, tx)))
        case _                                                                                => None
      }

    def scriptResult(cf: FailedTransactionError, tx: Transaction): Option[InvokeScriptResult] =
      tx match {
        case _: InvokeScriptTransaction => Some(InvokeScriptResult(error = Some(ErrorMessage(cf.code, cf.message))))
        case _                          => None
      }
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
    def traced: TracedResult[E, A] = TracedResult.wrapE(ei)
  }

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError {
    override def toString: String = s"TransactionValidationError(cause = $cause,\ntx = ${Json.prettyPrint(tx.json())})"
  }

  private val stats = TxProcessingStats
}
