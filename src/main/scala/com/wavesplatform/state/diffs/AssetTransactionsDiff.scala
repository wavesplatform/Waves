package com.wavesplatform.state.diffs

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{AssetInfo, Blockchain, Diff, LeaseBalance, Portfolio, SponsorshipValue}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.{AssetId, ProvenTransaction, ValidationError}

import scala.util.{Left, Right}

object AssetTransactionsDiff {
  def issue(height: Int)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    val info = AssetInfo(isReissuable = tx.reissuable, volume = tx.quantity, script = tx.script)
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.id() -> tx.quantity))),
        assetInfos = Map(tx.id()             -> info)
      ))
  }

  def reissue(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: ReissueTransaction): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.assetId, issuerOnly = true).flatMap { _ =>
      val oldInfo = blockchain.assetDescription(tx.assetId).get
      def wasBurnt = blockchain.addressTransactions(tx.sender, Set(BurnTransaction.typeId), Int.MaxValue, 0).exists {
        case (_, t: BurnTransaction) if t.assetId == tx.assetId => true
        case _                                                  => false
      }
      val isDataTxActivated = blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, blockchain.height)
      if (oldInfo.reissuable || (blockTime <= settings.allowInvalidReissueInSameBlockUntilTimestamp) || (!isDataTxActivated && wasBurnt)) {
        if ((Long.MaxValue - tx.quantity) < oldInfo.totalVolume && isDataTxActivated) {
          Left(GenericError("Asset total value overflow"))
        } else {
          Right(
            Diff(
              height = height,
              tx = tx,
              portfolios =
                Map(tx.sender.toAddress   -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.assetId -> tx.quantity))),
              assetInfos = Map(tx.assetId -> AssetInfo(volume = tx.quantity, isReissuable = tx.reissuable, script = None))
            ))
        }
      } else {
        Left(GenericError("Asset is not reissuable"))
      }
    }

  def burn(blockchain: Blockchain, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
    val burnAnyTokensEnabled = blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens, blockchain.height)

    validateAsset(tx, blockchain, tx.assetId, !burnAnyTokensEnabled).map { _ =>
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.assetId -> -tx.quantity))),
        assetInfos = Map(tx.assetId          -> AssetInfo(isReissuable = true, volume = -tx.quantity, None))
      )
    }
  }

  def sponsor(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: SponsorFeeTransaction): Either[ValidationError, Diff] = {
    validateAsset(tx, blockchain, tx.assetId, true).flatMap { _ =>
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
          sponsorship = Map(tx.assetId         -> SponsorshipValue(tx.minSponsoredAssetFee.getOrElse(0)))
        ))
    }
  }

  private def validateAsset(tx: ProvenTransaction, blockchain: Blockchain, assetId: AssetId, issuerOnly: Boolean): Either[ValidationError, Unit] = {
    blockchain.transactionInfo(assetId) match {
      case Some((_, sitx: IssueTransaction)) if sitx.script.isEmpty && !validIssuer(issuerOnly, tx.sender, sitx.sender) =>
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
}
