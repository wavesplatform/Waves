package com.wavesplatform.state.diffs

import com.wavesplatform.account.PublicKey
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{AssetInfo, Blockchain, Diff, LeaseBalance, Portfolio, SponsorshipValue}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.ProvenTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._

import scala.util.{Left, Right}

object AssetTransactionsDiff {
  def issue(blockchain: Blockchain, height: Int)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    val info  = AssetInfo(isReissuable = tx.reissuable, volume = tx.quantity)
    val asset = IssuedAsset(tx.id())
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(asset -> tx.quantity))),
        assetInfos = Map(asset               -> info),
        assetScripts = Map(asset -> tx.script).filter(_._2.isDefined),
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
        scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx)
      ))
  }

  def setAssetScript(blockchain: Blockchain, height: Int, blockTime: Long)(tx: SetAssetScriptTransaction): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      if (blockchain.hasAssetScript(tx.asset)) {
        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
            assetScripts = Map(tx.asset          -> tx.script),
            scriptsRun =
              // Asset script doesn't count before Ride4DApps activation
              if (blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, height)) {
                DiffsCommon.countScriptRuns(blockchain, tx)
              } else {
                Some(tx.sender.toAddress).count(blockchain.hasScript)
              },
            scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx)
          ))
      } else {
        Left(GenericError("Cannot set script on an asset issued without a script"))
      }
    }

  def reissue(blockchain: Blockchain, height: Int, blockTime: Long)(tx: ReissueTransaction): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      val oldInfo = blockchain.assetDescription(tx.asset).get

      val isDataTxActivated = blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, height)
      if (oldInfo.reissuable || (blockTime <= blockchain.settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp)) {
        if ((Long.MaxValue - tx.quantity) < oldInfo.totalVolume && isDataTxActivated) {
          Left(GenericError("Asset total value overflow"))
        } else {
          Right(
            Diff(
              height = height,
              tx = tx,
              portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.asset -> tx.quantity))),
              assetInfos = Map(tx.asset            -> AssetInfo(volume = tx.quantity, isReissuable = tx.reissuable)),
              scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
              scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx)
            ))
        }
      } else {
        Left(GenericError("Asset is not reissuable"))
      }
    }

  def burn(blockchain: Blockchain, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
    val burnAnyTokensEnabled = blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens, height)

    validateAsset(tx, blockchain, tx.asset, !burnAnyTokensEnabled).map { _ =>
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.asset -> -tx.quantity))),
        assetInfos = Map(tx.asset            -> AssetInfo(isReissuable = true, volume = -tx.quantity)),
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
        scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx)
      )
    }
  }

  def sponsor(blockchain: Blockchain, height: Int, blockTime: Long)(tx: SponsorFeeTransaction): Either[ValidationError, Diff] = {
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      Either.cond(
        !blockchain.hasAssetScript(tx.asset),
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
          sponsorship = Map(tx.asset           -> SponsorshipValue(tx.minSponsoredAssetFee.getOrElse(0))),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx),
          scriptsComplexity = DiffsCommon.countScriptsComplexity(blockchain, tx)
        ),
        GenericError("Sponsorship smart assets is disabled.")
      )
    }
  }

  private[this] def validateAsset(tx: ProvenTransaction,
                                  blockchain: Blockchain,
                                  assetId: IssuedAsset,
                                  issuerOnly: Boolean): Either[ValidationError, Unit] = {
    @inline
    def validIssuer(issuerOnly: Boolean, sender: PublicKey, issuer: PublicKey) =
      !issuerOnly || sender == issuer

    blockchain.transactionInfo(assetId.id) match {
      case Some((_, sitx: IssueTransaction)) if !validIssuer(issuerOnly, tx.sender, sitx.sender) =>
        Left(GenericError("Asset was issued by other address"))
      case None =>
        Left(GenericError("Referenced assetId not found"))
      case Some(_) =>
        Right({})
    }
  }
}
