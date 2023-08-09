package com.wavesplatform.state.diffs

import scala.util.Either.cond
import scala.collection.immutable.VectorMap
import com.wavesplatform.transaction.ERC20Address
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.state.diffs.DiffsCommon.*
import com.wavesplatform.lang.v1.ContractLimits.MaxExprSizeInBytes
import com.wavesplatform.state.*
import com.wavesplatform.features.EstimatorProvider.*
import com.google.protobuf.ByteString
import com.wavesplatform.lang.script.Script
import com.google.common.base.Utf8
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.lang.ValidationError
import cats.syntax.ior.*
import cats.syntax.flatMap.*
import cats.implicits.catsSyntaxEitherId

object AssetTransactionsDiffs {
  def updateInfo(b: Blockchain)(tx: UpdateAssetInfoTransaction): Either[ValidationError, StateSnapshot] =
    validateAsset(b, tx.assetId, tx.sender.toAddress, issuerOnly = true) >> {
      val minUpdateInfoInterval = b.settings.functionalitySettings.minAssetInfoUpdateInterval
      b.assetDescription(tx.assetId)
        .map(_.lastUpdatedAt)
        .toRight(GenericError("Asset doesn't exist"))
        .flatMap(lastUpdateHeight =>
          (tx.feeAsset match {
            case Waves =>
              Right(Portfolio(-tx.feeAmount.value))
            case IssuedAsset(asset) if b.isFeatureActivated(RideV6) =>
              Left(GenericError(s"Invalid fee asset: $asset, only Waves can be used to pay fees for UpdateAssetInfoTransaction"))
            case asset @ IssuedAsset(_) =>
              Right(Portfolio.build(asset -> -tx.feeAmount.value))
          })
            .flatMap { portfolioUpdate =>
              val updateAllowedHeight = lastUpdateHeight + minUpdateInfoInterval
              val updatedInfo         = AssetInfo(tx.name, tx.description, Height @@ b.height)
              if (b.height < updateAllowedHeight)
                Left(
                  GenericError(
                    s"Can't update info of asset with id=${tx.assetId.id} before $updateAllowedHeight block, " +
                      s"current height=${b.height}, minUpdateInfoInterval=$minUpdateInfoInterval"
                  )
                )
              else
                StateSnapshot.build(
                  b,
                  portfolios = Map(tx.sender.toAddress -> portfolioUpdate),
                  updatedAssets = Map(tx.assetId -> updatedInfo.leftIor)
                )
            }
        )
    }

  def sponsor(b: Blockchain)(tx: SponsorFeeTransaction): Either[ValidationError, StateSnapshot] =
    processSponsor(b, tx.sender.toAddress, tx.fee.value, SponsorFee(tx.asset.id, tx.minSponsoredAssetFee.map(_.value)))

  def burn(b: Blockchain)(tx: BurnTransaction): Either[ValidationError, StateSnapshot] =
    processBurn(b, tx.sender.toAddress, tx.fee.value, Burn(tx.asset.id, tx.quantity.value))

  def reissue(b: Blockchain, blockTime: Long)(tx: ReissueTransaction): Either[ValidationError, StateSnapshot] =
    processReissue(b, tx.sender.toAddress, blockTime, tx.fee.value, Reissue(tx.asset.id, tx.reissuable, tx.quantity.value))

  private def checkEstimationOverflow(b: Blockchain, script: Option[(Script, Long)]): Either[GenericError, Unit] =
    if (b.checkEstimationOverflow && script.exists(_._2 < 0))
      Left(GenericError(s"Unexpected negative complexity"))
    else
      Right(())

  def setAssetScript(b: Blockchain)(tx: SetAssetScriptTransaction): Either[ValidationError, StateSnapshot] =
    countVerifierComplexity(tx.script, b, isAsset = true)
      .flatMap(script =>
        validateAsset(b, tx.asset, tx.sender.toAddress, issuerOnly = true)
          .flatMap(_ =>
            (if (!b.hasAssetScript(tx.asset)) Left(GenericError("Cannot set script on an asset issued without a script"))
             else checkEstimationOverflow(b, script))
              .flatMap(_ =>
                checkSize(b, tx.script)
                  .flatMap { _ =>
                    StateSnapshot.build(
                      b,
                      assetScripts = Map(tx.asset -> script.map(AssetScriptInfo.tupled)),
                      portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee.value))
                    )
                  }
              )
          )
      )

  private def checkSize(b: Blockchain, scriptOpt: Option[Script]): Either[GenericError, Unit] =
    scriptOpt.fold(().asRight[GenericError]) { script =>
      cond(
        !b.isFeatureActivated(BlockRewardDistribution) || script.bytes().size <= MaxExprSizeInBytes,
        (),
        GenericError("Invalid script")
      )
    }

  def issue(b: Blockchain)(tx: IssueTransaction): Either[ValidationError, StateSnapshot] = { // TODO: unify with InvokeScript action diff?
    // First 20 bytes of id should be unique
    def requireUnique(): Boolean =
      b.resolveERC20Address(ERC20Address(tx.asset)).isEmpty

    def requireValidUtf(): Boolean = {
      def isValid(str: ByteString): Boolean = {
        val wellFormed  = Utf8.isWellFormed(str.toByteArray)
        val convertible = str == ByteString.copyFromUtf8(str.toStringUtf8)
        wellFormed && convertible
      }
      (isValid(tx.name) && isValid(tx.description)) || !b.isFeatureActivated(BlockV5)
    }

    val assetInfo   = AssetInfo(tx.name, tx.description, Height @@ b.height)
    val assetVolume = AssetVolumeInfo(tx.reissuable, BigInt(tx.quantity.value))
    val assetStatic = AssetStaticInfo(TransactionId @@ tx.id(), TransactionId @@ tx.id(), tx.sender, tx.decimals.value, b.isNFT(tx))

    cond(requireUnique(), (), GenericError(s"Asset ${tx.asset} is already issued"))
      .flatMap(_ =>
        cond(requireValidUtf(), (), GenericError("Valid UTF-8 strings required"))
          .flatMap(_ =>
            checkSize(b, tx.script)
              .flatMap { _ =>
                val asset = IssuedAsset(tx.id())
                countVerifierComplexity(tx.script, b, isAsset = true)
                  .flatTap(checkEstimationOverflow(b, _))
                  .flatMap(script =>
                    StateSnapshot.build(
                      b,
                      assetScripts = Map(asset -> script.map(AssetScriptInfo.tupled)),
                      issuedAssets = VectorMap(asset -> NewAssetInfo(assetStatic, assetInfo, assetVolume)),
                      portfolios = VectorMap(tx.sender.toAddress -> Portfolio.build(-tx.fee.value, asset, tx.quantity.value))
                    )
                  )
              }
          )
      )
  }
}
