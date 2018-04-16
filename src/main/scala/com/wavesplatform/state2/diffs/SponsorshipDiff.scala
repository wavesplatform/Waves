package com.wavesplatform.state2.diffs

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{AssetInfo, Diff, LeaseBalance, Portfolio, SponsorshipValue}
import scorex.account.PublicKeyAccount
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.{IssueTransaction, SponsorFeeTransaction, CancelFeeSponsorshipTransaction}
import scorex.transaction.{AssetId, ProvenTransaction, ValidationError}

import scala.util.{Left, Right}

object SponsorshipTransactionsDiff {

  def sponsor(state: SnapshotStateReader, settings: FunctionalitySettings, blockTime: Long, height: Int, fp: FeatureProvider)(
      tx: SponsorFeeTransaction): Either[ValidationError, Diff] = {
    validate(tx, state, tx.assetId, fp).flatMap { _ =>
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
    validate(tx, state, tx.assetId, fp).flatMap { _ =>
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)) /*,
          sponsorship = Map(tx.assetId         -> SponsorshipValue(0))*/
        ))
    }
  }

  private def validate(tx: ProvenTransaction, state: SnapshotStateReader, assetId: AssetId, fp: FeatureProvider): Either[ValidationError, Unit] = {
    state.transactionInfo(assetId) match {
      case Some((_, itx: IssueTransaction)) if !validIssuer(true, tx.sender, itx.sender) =>
        Left(GenericError("Asset was issued by other address"))
      case None =>
        Left(GenericError("Referenced assetId not found"))
      case Some(_) => Right(())
    }
  }

  private def validIssuer(issuerOnly: Boolean, sender: PublicKeyAccount, issuer: PublicKeyAccount): Boolean = {
    if (issuerOnly) sender equals issuer
    else true
  }
}
