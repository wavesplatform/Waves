package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.traits.domain.{Burn, Reissue}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._

object AssetTransactionsDiff {
  def issue(blockchain: Blockchain)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    val staticInfo = AssetStaticInfo(TransactionId @@ tx.id(), tx.sender, tx.decimals, tx.quantity == 1 && tx.reissuable == false && tx.decimals == 0)
    val volumeInfo = AssetVolumeInfo(tx.reissuable, BigInt(tx.quantity))
    val info       = AssetInfo(new String(tx.name), new String(tx.description), Height @@ blockchain.height)

    val asset = IssuedAsset(tx.id())

    DiffsCommon
      .countScriptComplexity(tx.script, blockchain)
      .map(
        script =>
          Diff(
            tx = tx,
            portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(asset -> tx.quantity))),
            issuedAssets = Map(asset             -> ((staticInfo, info, volumeInfo))),
            assetScripts = Map(asset             -> script.map(script => ((tx.sender, script._1, script._2)))),
            scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
          )
      )
  }

  def setAssetScript(blockchain: Blockchain, blockTime: Long)(tx: SetAssetScriptTransaction): Either[ValidationError, Diff] =
    DiffsCommon.validateAsset(blockchain, tx.asset, tx.sender, issuerOnly = true).flatMap { _ =>
      if (blockchain.hasAssetScript(tx.asset)) {
        DiffsCommon
          .countScriptComplexity(tx.script, blockchain)
          .map(
            script =>
              Diff(
                tx = tx,
                portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
                assetScripts = Map(tx.asset          -> script.map(script => ((tx.sender, script._1, script._2)))),
                scriptsRun =
                  // Asset script doesn't count before Ride4DApps activation
                  if (blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, blockchain.height)) {
                    DiffsCommon.countScriptRuns(blockchain, tx)
                  } else {
                    Some(tx.sender.toAddress).count(blockchain.hasScript)
                  }
              )
          )
      } else {
        Left(GenericError("Cannot set script on an asset issued without a script"))
      }
    }

  def reissue(blockchain: Blockchain, blockTime: Long)(tx: ReissueTransaction): Either[ValidationError, Diff] =
    DiffsCommon
      .processReissue(
        blockchain,
        tx.sender,
        blockTime,
        tx.fee,
        Reissue(tx.asset.id, tx.reissuable, tx.quantity)
      )
      .map(Diff(tx = tx, scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)) |+| _)

  def burn(blockchain: Blockchain)(tx: BurnTransaction): Either[ValidationError, Diff] =
    DiffsCommon
      .processBurn(
        blockchain,
        tx.sender,
        tx.fee,
        Burn(tx.asset.id, tx.quantity)
      )
      .map(Diff(tx = tx, scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)) |+| _)

  def sponsor(blockchain: Blockchain, blockTime: Long)(tx: SponsorFeeTransaction): Either[ValidationError, Diff] =
    DiffsCommon.validateAsset(blockchain, tx.asset, tx.sender, issuerOnly = true).flatMap { _ =>
      Either.cond(
        !blockchain.hasAssetScript(tx.asset),
        Diff(
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
          sponsorship = Map(tx.asset           -> SponsorshipValue(tx.minSponsoredAssetFee.getOrElse(0))),
          scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
        ),
        GenericError("Sponsorship smart assets is disabled.")
      )
    }

  def updateInfo(blockchain: Blockchain)(tx: UpdateAssetInfoTransaction): Either[ValidationError, Diff] =
    DiffsCommon.validateAsset(blockchain, tx.assetId, tx.sender, issuerOnly = true) >> {
      lazy val portfolioUpdate = tx.feeAsset match {
        case ia @ IssuedAsset(_) => Portfolio(0L, LeaseBalance.empty, Map(ia -> -tx.feeAmount))
        case Asset.Waves         => Portfolio(balance = -tx.feeAmount, LeaseBalance.empty, Map.empty)
      }

      val minUpdateInfoInterval = blockchain.settings.functionalitySettings.minAssetInfoUpdateInterval

      for {
        lastUpdateHeight <- blockchain
          .assetDescription(tx.assetId)
          .map(_.lastUpdatedAt)
          .toRight(GenericError("Asset doesn't exist"))
        updateAllowedAt = lastUpdateHeight + minUpdateInfoInterval
        _ <- Either.cond(
          updateAllowedAt < blockchain.height,
          (),
          GenericError(s"Can't update asset info before $updateAllowedAt block")
        )
        updatedInfo = AssetInfo(tx.name, tx.description, Height @@ blockchain.height)
      } yield Diff(
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> portfolioUpdate),
        updatedAssets = Map(tx.assetId       -> updatedInfo.leftIor),
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
      )
    }
}
