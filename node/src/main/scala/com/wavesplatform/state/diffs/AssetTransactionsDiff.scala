package com.wavesplatform.state.diffs

import cats.instances.either._
import cats.syntax.flatMap._
import cats.syntax.ior._
import cats.syntax.semigroup._
import com.google.common.base.Utf8
import com.google.protobuf.ByteString
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.traits.domain.{Burn, Reissue, SponsorFee}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._
import com.wavesplatform.utils.ScorexLogging

object AssetTransactionsDiff extends ScorexLogging {
  def issue(blockchain: Blockchain)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    def requireValidUtf(): Boolean = {
      def isValid(str: ByteString): Boolean = {
        val convertible = ByteString.copyFromUtf8(str.toStringUtf8) == str
        val wellFormed  = Utf8.isWellFormed(str.toByteArray)
        convertible && wellFormed
      }
      val activated = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)
      !activated || (isValid(tx.name) && isValid(tx.description))
    }

    val staticInfo = AssetStaticInfo(TransactionId @@ tx.id(), tx.sender, tx.decimals, blockchain.isNFT(tx))
    val volumeInfo = AssetVolumeInfo(tx.reissuable, BigInt(tx.quantity))
    val info       = AssetInfo(tx.name, tx.description, Height @@ blockchain.height)

    val asset = IssuedAsset(tx.id())

    for {
      _ <- Either.cond(requireValidUtf(), (), GenericError("Valid UTF-8 strings required"))
      result <- DiffsCommon
        .countVerifierComplexity(tx.script, blockchain, isAsset = true)
        .map(
          script =>
            Diff(
              portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(asset -> tx.quantity))),
              issuedAssets = Map(asset             -> NewAssetInfo(staticInfo, info, volumeInfo)),
              assetScripts = Map(asset             -> script.map(AssetScriptInfo.tupled)),
              scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
            )
        )
    } yield result
  }

  def setAssetScript(blockchain: Blockchain)(tx: SetAssetScriptTransaction): Either[ValidationError, Diff] =
    DiffsCommon.validateAsset(blockchain, tx.asset, tx.sender.toAddress, issuerOnly = true).flatMap { _ =>
      if (blockchain.hasAssetScript(tx.asset)) {
        DiffsCommon
          .countVerifierComplexity(tx.script, blockchain, isAsset = true)
          .map { script =>
            Diff(
              portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
              assetScripts = Map(tx.asset          -> script.map(AssetScriptInfo.tupled)),
              scriptsRun =
                // Asset script doesn't count before Ride4DApps activation
                if (blockchain.isFeatureActivated(BlockchainFeatures.Ride4DApps, blockchain.height)) {
                  DiffsCommon.countScriptRuns(blockchain, tx)
                } else {
                  Some(tx.sender.toAddress).count(blockchain.hasAccountScript)
                }
            )
          }
      } else {
        Left(GenericError("Cannot set script on an asset issued without a script"))
      }
    }

  def reissue(blockchain: Blockchain, blockTime: Long)(tx: ReissueTransaction): Either[ValidationError, Diff] =
    DiffsCommon
      .processReissue(blockchain, tx.sender.toAddress, blockTime, tx.fee, Reissue(tx.asset.id, tx.reissuable, tx.quantity))
      .map(_ |+| Diff(scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)))

  def burn(blockchain: Blockchain)(tx: BurnTransaction): Either[ValidationError, Diff] =
    DiffsCommon
      .processBurn(blockchain, tx.sender.toAddress, tx.fee, Burn(tx.asset.id, tx.quantity))
      .map(_ |+| Diff(scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)))

  def sponsor(blockchain: Blockchain)(tx: SponsorFeeTransaction): Either[ValidationError, Diff] =
    DiffsCommon
      .processSponsor(blockchain, tx.sender.toAddress, tx.fee, SponsorFee(tx.asset.id, tx.minSponsoredAssetFee))
      .map(_ |+| Diff(scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)))

  def updateInfo(blockchain: Blockchain)(tx: UpdateAssetInfoTransaction): Either[ValidationError, Diff] =
    DiffsCommon.validateAsset(blockchain, tx.assetId, tx.sender.toAddress, issuerOnly = true) >> {
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
          blockchain.height >= updateAllowedAt,
          (),
          GenericError(
            s"Can't update info of asset with id=${tx.assetId.id} before $updateAllowedAt block, " +
              s"current height=${blockchain.height}, minUpdateInfoInterval=$minUpdateInfoInterval"
          )
        )
        updatedInfo = AssetInfo(tx.name, tx.description, Height @@ blockchain.height)
      } yield Diff(
        portfolios = Map(tx.sender.toAddress -> portfolioUpdate),
        updatedAssets = Map(tx.assetId       -> updatedInfo.leftIor),
        scriptsRun = DiffsCommon.countScriptRuns(blockchain, tx)
      )
    }
}
