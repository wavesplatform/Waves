package com.wavesplatform.state.diffs

import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{AssetInfo, Blockchain, Diff, LeaseBalance, Portfolio, SponsorshipValue}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.{ProvenTransaction, ValidationError}

import scala.util.{Left, Right}

object AssetTransactionsDiff {
  def issue(height: Int)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    val info  = AssetInfo(isReissuable = tx.reissuable, volume = tx.quantity)
    val asset = IssuedAsset(tx.id())
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(asset -> tx.quantity))),
        assetInfos = Map(asset               -> info),
        assetScripts = if (tx.script.isEmpty) { Map() } else { Map(asset -> tx.script) }
      ))
  }

  def setAssetScript(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: SetAssetScriptTransaction): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      if (blockchain.hasAssetScript(tx.asset)) {
        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
            assetScripts = Map(tx.asset          -> tx.script)
          ))
      } else {
        Left(GenericError("Cannot set script on an asset issued without a script"))
      }
    }

  def reissue(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: ReissueTransaction): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      val oldInfo = blockchain.assetDescription(tx.asset).get

      def wasBurnt =
        blockchain
          .addressTransactions(tx.sender, Set(BurnTransaction.typeId), Int.MaxValue, None)
          .getOrElse(Seq.empty)
          .exists {
            case (_, t: BurnTransaction) if t.asset == tx.asset => true
            case _                                              => false
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
              portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.asset -> tx.quantity))),
              assetInfos = Map(tx.asset            -> AssetInfo(volume = tx.quantity, isReissuable = tx.reissuable))
            ))
        }
      } else {
        Left(GenericError("Asset is not reissuable"))
      }
    }

  def burn(blockchain: Blockchain, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
    val burnAnyTokensEnabled = blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens, blockchain.height)

    validateAsset(tx, blockchain, tx.asset, !burnAnyTokensEnabled).map { _ =>
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.asset -> -tx.quantity))),
        assetInfos = Map(tx.asset            -> AssetInfo(isReissuable = true, volume = -tx.quantity))
      )
    }
  }

  def sponsor(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: SponsorFeeTransaction): Either[ValidationError, Diff] = {
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      Either.cond(
        !blockchain.hasAssetScript(tx.asset),
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
          sponsorship = Map(tx.asset           -> SponsorshipValue(tx.minSponsoredAssetFee.getOrElse(0)))
        ),
        GenericError("Sponsorship smart assets is disabled.")
      )
    }
  }

  private def validateAsset(tx: ProvenTransaction,
                            blockchain: Blockchain,
                            assetId: IssuedAsset,
                            issuerOnly: Boolean): Either[ValidationError, Unit] = {
    blockchain.transactionInfo(assetId.id) match {
      case Some((_, sitx: IssueTransaction)) if !validIssuer(issuerOnly, tx.sender, sitx.sender) =>
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
