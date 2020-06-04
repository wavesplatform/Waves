package com.wavesplatform.state.diffs

import cats.instances.either._
import cats.instances.map._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.functor._
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.features.BlockchainFeatures.BlockV5
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.metrics.TxProcessingStats
import com.wavesplatform.metrics.TxProcessingStats.TxTimerExt
import com.wavesplatform.state.InvokeScriptResult.ErrorMessage
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
import com.wavesplatform.state.{Blockchain, Diff, InvokeScriptResult, LeaseBalance, NewTransactionInfo, Portfolio, Sponsorship}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{FailedTransactionError, GenericError, UnsupportedTransactionType}
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
      case isFailedTransaction(error) if acceptFailed(blockchain) => failedTransactionDiff(blockchain, tx, error).traced
      case result                                                 => result
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
      _    <- if (verify) validateCommon(blockchain, tx, prevBlockTimestamp, currentBlockTimestamp).traced else success
      _    <- validateFunds(blockchain, tx).traced
      _    <- if (verify) validateProofs(blockchain, tx) else success
      diff <- transactionDiff(blockchain, tx, currentBlockTimestamp, skipFailing)
      _    <- validateBalance(blockchain, tx.typeId, diff).traced
      _    <- if (verifyAssets) validateAssets(blockchain, tx) else success
    } yield diff
    result.leftMap(TransactionValidationError(_, tx))
  }

  // validation related
  private def validateCommon(
      blockchain: Blockchain,
      tx: Transaction,
      prevBlockTs: Option[Long],
      currentBlockTs: Long
  ): Either[ValidationError, Unit] =
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
      }

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

  private def validateProofs(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Unit] = Verifier(blockchain)(tx).as(())

  private def validateAssets(blockchain: Blockchain, tx: Transaction): TracedResult[ValidationError, Unit] = Verifier.assets(blockchain)(tx).as(())

  private def validateBalance(blockchain: Blockchain, txType: TxType, diff: Diff): Either[ValidationError, Unit] =
    stats.balanceValidation.measureForType(txType)(BalanceDiffValidation(blockchain)(diff).as(()))

  // diff making related
  private def transactionDiff(
      blockchain: Blockchain,
      tx: Transaction,
      currentBlockTs: Long,
      skipFailing: Boolean
  ): TracedResult[ValidationError, Diff] =
    stats.transactionDiffValidation.measureForType(tx.typeId) {
      tx match {
        case gtx: GenesisTransaction     => GenesisTransactionDiff(blockchain.height)(gtx).traced
        case ptx: PaymentTransaction     => PaymentTransactionDiff(blockchain)(ptx).traced
        case ci: InvokeScriptTransaction => InvokeScriptTransactionDiff(blockchain, currentBlockTs, skipExecution = skipFailing)(ci)
        case etx: ExchangeTransaction    => ExchangeTransactionDiff(blockchain)(etx).traced
        case ptx: ProvenTransaction      => provenTransactionDiff(blockchain, currentBlockTs)(ptx)
        case _                           => UnsupportedTransactionType.asLeft.traced
      }
    }

  private def provenTransactionDiff(blockchain: Blockchain, currentBlockTs: Long)(tx: ProvenTransaction): TracedResult[ValidationError, Diff] = {
    val diff = tx match {
      case itx: IssueTransaction             => AssetTransactionsDiff.issue(blockchain)(itx)
      case rtx: ReissueTransaction           => AssetTransactionsDiff.reissue(blockchain, currentBlockTs)(rtx)
      case btx: BurnTransaction              => AssetTransactionsDiff.burn(blockchain)(btx)
      case uaitx: UpdateAssetInfoTransaction => AssetTransactionsDiff.updateInfo(blockchain)(uaitx)
      case ttx: TransferTransaction          => TransferTransactionDiff(blockchain, currentBlockTs)(ttx)
      case mtx: MassTransferTransaction      => MassTransferTransactionDiff(blockchain, currentBlockTs)(mtx)
      case ltx: LeaseTransaction             => LeaseTransactionsDiff.lease(blockchain)(ltx)
      case ltx: LeaseCancelTransaction       => LeaseTransactionsDiff.leaseCancel(blockchain, currentBlockTs)(ltx)
      case atx: CreateAliasTransaction       => CreateAliasTransactionDiff(blockchain)(atx)
      case dtx: DataTransaction              => DataTransactionDiff(blockchain)(dtx)
      case sstx: SetScriptTransaction        => SetScriptTransactionDiff(blockchain)(sstx)
      case sstx: SetAssetScriptTransaction   => AssetTransactionsDiff.setAssetScript(blockchain, currentBlockTs)(sstx)
      case stx: SponsorFeeTransaction        => AssetTransactionsDiff.sponsor(blockchain)(stx)
      case _                                 => UnsupportedTransactionType.asLeft
    }
    val complexityDiff = Diff.empty.copy(scriptsComplexity = DiffsCommon.getScriptsComplexity(blockchain, tx)).asRight[ValidationError]

    Monoid.combine(complexityDiff, diff).traced
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

  private def failedTransactionDiff(blockchain: Blockchain, tx: Transaction, error: Option[ErrorMessage]): Either[ValidationError, Diff] = {
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
        scriptResults = Map(tx.id() -> InvokeScriptResult(error = error))
      )
    }
  }

  private object isFailedTransaction {
    def unapply(result: TracedResult[ValidationError, Diff]): Option[Option[ErrorMessage]] =
      result match {
        case TracedResult(Left(TransactionValidationError(e: FailedTransactionError, tx)), _) => Some(errorMessage(e, tx))
        case _                                                                           => None
      }

    def errorMessage(cf: FailedTransactionError, tx: Transaction): Option[ErrorMessage] =
      tx match {
        case _: InvokeScriptTransaction => Some(ErrorMessage(cf.cause.code, cf.cause.error))
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

  private val success: TracedResult[ValidationError, Unit] = Right(()).traced

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError {
    override def toString: String = s"TransactionValidationError(cause = $cause,\ntx = ${Json.prettyPrint(tx.json())})"
  }

  private val stats = TxProcessingStats
}
