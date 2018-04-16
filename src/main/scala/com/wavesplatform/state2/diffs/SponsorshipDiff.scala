package com.wavesplatform.state2.diffs

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{AssetInfo, Diff, LeaseBalance, Portfolio, SponsorshipValue}
import scorex.account.PublicKeyAccount
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.{IssueTransaction, SponsorFeeTransaction, CancelFeeSponsorshipTransaction}
import scorex.transaction.{AssetId, SignedTransaction, ValidationError}

import scala.util.{Left, Right}

object SponsorshipTransactionsDiff {

  def sponsor(state: SnapshotStateReader, settings: FunctionalitySettings, blockTime: Long, height: Int, fp: FeatureProvider)(
      tx: SponsorFeeTransaction): Either[ValidationError, Diff] = {
    validateAsset(tx, state, tx.assetId).flatMap { _ =>
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)) /*,
          sponsorship = Map(tx.assetId         -> SponsorshipValue(tx.minFee))*/
        ))
    }
  }

  def cancel(state: SnapshotStateReader, settings: FunctionalitySettings, blockTime: Long, height: Int, fp: FeatureProvider)(
      tx: CancelFeeSponsorshipTransaction): Either[ValidationError, Diff] = {
    validateAsset(tx, state, tx.assetId).flatMap { _ =>
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)) /*,
          sponsorship = Map(tx.assetId         -> SponsorshipValue(0))*/
        ))
    }
  }

  private def validateAsset(tx: SignedTransaction, state: SnapshotStateReader, assetId: AssetId): Either[ValidationError, Unit] = {
    state.transactionInfo(assetId) match {
      case Some((_, itx: IssueTransaction)) if !validIssuer(true, tx.sender, itx.sender) =>
        Left(GenericError("Asset was issued by other address"))
      case None =>
        Left(GenericError("Referenced assetId not found"))
      case Some(_) =>
        Right({})
    }
  }

  private def validIssuer(issuerOnly: Boolean, sender: PublicKeyAccount, issuer: PublicKeyAccount): Boolean = {
    if (issuerOnly) sender equals issuer
    else true
  }

  /*  def reissue(state: SnapshotStateReader, settings: FunctionalitySettings, blockTime: Long, height: Int, fp: FeatureProvider)(
      tx: ReissueTransaction): Either[ValidationError, Diff] =
    validateAsset(tx, state, tx.assetId, issuerOnly = true).flatMap { _ =>
      val oldInfo = state.assetDescription(tx.assetId).get
      if (!oldInfo.reissuable && blockTime > settings.allowInvalidReissueInSameBlockUntilTimestamp) {
        Left(
          GenericError(s"Asset is not reissuable and blockTime=$blockTime is greater than " +
            s"settings.allowInvalidReissueInSameBlockUntilTimestamp=${settings.allowInvalidReissueInSameBlockUntilTimestamp}"))
      } else if ((Long.MaxValue - tx.quantity) < oldInfo.totalVolume && fp.isFeatureActivated(BlockchainFeatures.BurnAnyTokens, state.height)) {
        Left(GenericError(s"Asset total value overflow"))
      } else {
        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.assetId -> tx.quantity))),
            assetInfos = Map(tx.assetId          -> AssetInfo(volume = tx.quantity, isReissuable = tx.reissuable, script = None))
          ))
      }
    }

  def burn(state: SnapshotStateReader, fp: FeatureProvider, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
    val burnAnyTokensEnabled = fp.isFeatureActivated(BlockchainFeatures.BurnAnyTokens, state.height)

    validateAsset(tx, state, tx.assetId, !burnAnyTokensEnabled).map(itx => {
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.assetId -> -tx.amount))),
        assetInfos = Map(tx.assetId          -> AssetInfo(isReissuable = true, volume = -tx.amount, None))
      )
    })
  }
 */
}
