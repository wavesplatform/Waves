package com.wavesplatform.state.diffs

import cats.instances.map._
import cats.syntax.semigroup._
import com.wavesplatform.account.{Address, Recipient}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{Asset, TxValidationError}

import scala.util.Right
import scala.util.control.NonFatal

object TransferDiff {
  def apply(
      blockchain: Blockchain
  )(senderAddress: Address, recipient: Recipient, amount: Long, assetId: Asset, fee: Long, feeAssetId: Asset): Either[ValidationError, Diff] = {

    val isSmartAsset = feeAssetId match {
      case Waves => false
      case asset @ IssuedAsset(_) =>
        blockchain
          .assetDescription(asset)
          .flatMap(_.script)
          .isDefined
    }

    for {
      recipient <- blockchain.resolveAlias(recipient)
      _         <- Either.cond(!isSmartAsset, (), GenericError("Smart assets can't participate in TransferTransactions as a fee"))

      _ <- validateOverflow(blockchain, blockchain.height, amount, fee)
      portfolios = (assetId match {
        case Waves =>
          Map(senderAddress -> Portfolio(-amount, LeaseBalance.empty, Map.empty)).combine(
            Map(recipient -> Portfolio(amount, LeaseBalance.empty, Map.empty))
          )
        case asset @ IssuedAsset(_) =>
          Map(senderAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -amount))).combine(
            Map(recipient -> Portfolio(0, LeaseBalance.empty, Map(asset -> amount)))
          )
      }).combine(
        feeAssetId match {
          case Waves => Map(senderAddress -> Portfolio(-fee, LeaseBalance.empty, Map.empty))
          case asset @ IssuedAsset(_) =>
            val senderPf = Map(senderAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -fee)))
            if (blockchain.height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)) {
              val sponsorPf = blockchain
                .assetDescription(asset)
                .collect {
                  case desc if desc.sponsorship > 0 =>
                    val feeInWaves = Sponsorship.toWaves(fee, desc.sponsorship)
                    Map[Address, Portfolio](desc.issuer.toAddress -> Portfolio(-feeInWaves, LeaseBalance.empty, Map(asset -> fee)))
                }
                .getOrElse(Map.empty)
              senderPf.combine(sponsorPf)
            } else senderPf
        }
      )
      assetIssued    = assetId.fold(true)(blockchain.assetDescription(_).isDefined)
      feeAssetIssued = feeAssetId.fold(true)(blockchain.assetDescription(_).isDefined)
      _ <- Either.cond(
        blockchain.lastBlockTimestamp
          .forall(_ <= blockchain.settings.functionalitySettings.allowUnissuedAssetsUntil || (assetIssued && feeAssetIssued)),
        (),
        GenericError(
          s"Unissued assets are not allowed after allowUnissuedAssetsUntil=${blockchain.settings.functionalitySettings.allowUnissuedAssetsUntil}"
        )
      )
    } yield Diff(portfolios = portfolios)
  }

  private def validateOverflow(blockchain: Blockchain, height: Int, amount: Long, fee: Long) =
    if (blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, height))
      Right(()) // lets transaction validates itself
    else
      try {
        Math.addExact(fee, amount)
        Right(())
      } catch {
        case NonFatal(_) => Left(TxValidationError.OverflowError)
      }
}
