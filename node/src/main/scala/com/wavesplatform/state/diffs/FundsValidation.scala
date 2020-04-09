package com.wavesplatform.state.diffs

import cats.instances.either._
import cats.instances.map._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.functor._
import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures.AcceptFailedScriptTransaction
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio, Sponsorship}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, UnsupportedTransactionType}
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.lease.LeaseCancelTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{GenesisTransaction, PaymentTransaction, ProvenTransaction, Transaction}

object FundsValidation {
  def apply(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Unit] = {
    val skip = tx match {
      case _: LeaseCancelTransaction                                                                                            => true
      case _: InvokeScriptTransaction | _: ExchangeTransaction if !blockchain.isFeatureActivated(AcceptFailedScriptTransaction) => true
      case _                                                                                                                    => false
    }

    if (skip) Right(())
    else {
      for {
        fee <- feePortfolios(blockchain, tx)
        _   <- BalanceDiffValidation(blockchain)(Diff(tx, portfolios = fee))
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
    }
  }

  def feePortfolios(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Map[Address, Portfolio]] = {
    tx match {
      case gtx: GenesisTransaction =>
        Map(gtx.recipient -> Portfolio(balance = gtx.amount, LeaseBalance.empty, assets = Map.empty)).asRight
      case ptx: PaymentTransaction =>
        Monoid
          .combine(
            Map(ptx.recipient        -> Portfolio(balance = ptx.amount, LeaseBalance.empty, assets = Map.empty)),
            Map(ptx.sender.toAddress -> Portfolio(balance = -ptx.amount - ptx.fee, LeaseBalance.empty, assets = Map.empty))
          )
          .asRight
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
  }

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
      _ <- BalanceDiffValidation(blockchain)(orderDiff)
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
}
