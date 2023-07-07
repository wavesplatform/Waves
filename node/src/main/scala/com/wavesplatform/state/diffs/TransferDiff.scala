package com.wavesplatform.state.diffs

import cats.implicits.toBifunctorOps
import com.wavesplatform.account.{Address, AddressOrAlias}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, TxValidationError}

import scala.util.control.NonFatal

object TransferTransactionDiff {
  def apply(blockchain: Blockchain)(tx: TransferTransaction): Either[ValidationError, StateSnapshot] =
    TransferDiff(blockchain)(tx.sender.toAddress, tx.recipient, tx.amount.value, tx.assetId, tx.fee.value, tx.feeAssetId)
}

object TransferDiff {
  def apply(
      blockchain: Blockchain
  )(
      senderAddress: Address,
      recipient: AddressOrAlias,
      amount: Long,
      assetId: Asset,
      fee: Long,
      feeAssetId: Asset
  ): Either[ValidationError, StateSnapshot] = {

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
      transferPf <- assetId match {
        case Waves =>
          Portfolio
            .combine(
              Map(senderAddress -> Portfolio(-amount)),
              Map(recipient     -> Portfolio(amount))
            )
            .leftMap(GenericError(_))
        case asset @ IssuedAsset(_) =>
          Portfolio
            .combine(
              Map(senderAddress -> Portfolio.build(asset -> -amount)),
              Map(recipient     -> Portfolio.build(asset -> amount))
            )
            .leftMap(GenericError(_))
      }
      feePf <- feeAssetId match {
        case Waves => Right(Map(senderAddress -> Portfolio(-fee)))
        case asset @ IssuedAsset(_) =>
          val senderPf = Map(senderAddress -> Portfolio.build(asset -> -fee))
          if (blockchain.height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)) {
            val sponsorPf = blockchain
              .assetDescription(asset)
              .collect {
                case desc if desc.sponsorship > 0 =>
                  val feeInWaves = Sponsorship.toWaves(fee, desc.sponsorship)
                  Map[Address, Portfolio](desc.issuer.toAddress -> Portfolio.build(-feeInWaves, asset, fee))
              }
              .getOrElse(Map.empty)
            Portfolio.combine(senderPf, sponsorPf).leftMap(GenericError(_))
          } else Right(senderPf)
      }
      portfolios <- Portfolio.combine(transferPf, feePf).leftMap(GenericError(_))
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
      snapshot <- StateSnapshot.build(blockchain, portfolios = portfolios)
    } yield snapshot
  }

  private def validateOverflow(blockchain: Blockchain, height: Int, amount: Long, fee: Long): Either[ValidationError, Unit] =
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
