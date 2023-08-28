package com.wavesplatform.state.diffs

import cats.implicits.catsSyntaxEitherId
import cats.syntax.ior.*
import com.google.common.base.Utf8
import com.google.protobuf.ByteString
import com.wavesplatform.features.BlockchainFeatures.*
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits.MaxExprSizeInBytes
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.state.*
import com.wavesplatform.state.diffs.DiffsCommon.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.ERC20Address
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.*

import scala.collection.immutable.VectorMap
import scala.util.Either.cond

object AssetTransactionsDiffs {
  def updateInfo(blockchain: Blockchain)(tx: UpdateAssetInfoTransaction): Either[ValidationError, StateSnapshot] =
    for {
      _ <- validateAsset(blockchain, tx.assetId, tx.sender.toAddress, issuerOnly = true)
      lastUpdateHeight <- blockchain
        .assetDescription(tx.assetId)
        .map(_.lastUpdatedAt)
        .toRight(GenericError("Asset doesn't exist"))
      minUpdateInfoInterval = blockchain.settings.functionalitySettings.minAssetInfoUpdateInterval
      updateAllowedHeight   = lastUpdateHeight + minUpdateInfoInterval
      updatedInfo           = AssetInfo(tx.name, tx.description, Height @@ blockchain.height)
      _ <- cond(
        blockchain.height >= updateAllowedHeight,
        (),
        GenericError(
          s"Can't update info of asset with id=${tx.assetId.id} before $updateAllowedHeight block, " +
            s"current height=${blockchain.height}, minUpdateInfoInterval=$minUpdateInfoInterval"
        )
      )
      feePortfolio <- tx.feeAsset match {
        case Waves =>
          Right(Portfolio(-tx.feeAmount.value))
        case IssuedAsset(asset) if blockchain.isFeatureActivated(RideV6) =>
          Left(GenericError(s"Invalid fee asset: $asset, only Waves can be used to pay fees for UpdateAssetInfoTransaction"))
        case asset @ IssuedAsset(_) =>
          Right(Portfolio.build(asset -> -tx.feeAmount.value))
      }
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = Map(tx.sender.toAddress -> feePortfolio),
        updatedAssets = Map(tx.assetId -> updatedInfo.leftIor)
      )
    } yield snapshot

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

  def setAssetScript(blockchain: Blockchain)(tx: SetAssetScriptTransaction): Either[ValidationError, StateSnapshot] =
    for {
      _      <- validateAsset(blockchain, tx.asset, tx.sender.toAddress, issuerOnly = true)
      script <- countVerifierComplexity(tx.script, blockchain, isAsset = true)
      _ <-
        if (!blockchain.hasAssetScript(tx.asset)) Left(GenericError("Cannot set script on an asset issued without a script"))
        else checkEstimationOverflow(blockchain, script)
      _ <- checkSize(blockchain, tx.script)
      snapshot <- StateSnapshot.build(
        blockchain,
        assetScripts = Map(tx.asset -> script.map(AssetScriptInfo.tupled)),
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee.value))
      )
    } yield snapshot

  private def checkSize(b: Blockchain, scriptOpt: Option[Script]): Either[GenericError, Unit] =
    scriptOpt.fold(().asRight[GenericError]) { script =>
      cond(
        !b.isFeatureActivated(BlockRewardDistribution) || script.bytes().size <= MaxExprSizeInBytes,
        (),
        GenericError("Invalid script")
      )
    }

  def issue(blockchain: Blockchain)(tx: IssueTransaction): Either[ValidationError, StateSnapshot] = { // TODO: unify with InvokeScript action diff?
    // First 20 bytes of id should be unique
    def requireUnique(): Boolean =
      blockchain.resolveERC20Address(ERC20Address(tx.asset)).isEmpty

    def requireValidUtf(): Boolean = {
      def isValid(str: ByteString): Boolean = {
        val wellFormed  = Utf8.isWellFormed(str.toByteArray)
        val convertible = str == ByteString.copyFromUtf8(str.toStringUtf8)
        wellFormed && convertible
      }
      (isValid(tx.name) && isValid(tx.description)) || !blockchain.isFeatureActivated(BlockV5)
    }

    val assetInfo   = AssetInfo(tx.name, tx.description, Height @@ blockchain.height)
    val assetVolume = AssetVolumeInfo(tx.reissuable, BigInt(tx.quantity.value))
    val assetStatic = AssetStaticInfo(TransactionId @@ tx.id(), TransactionId @@ tx.id(), tx.sender, tx.decimals.value, blockchain.isNFT(tx))
    val asset       = IssuedAsset(tx.id())

    for {
      _      <- cond(requireUnique(), (), GenericError(s"Asset ${tx.asset} is already issued"))
      _      <- cond(requireValidUtf(), (), GenericError("Valid UTF-8 strings required"))
      _      <- checkSize(blockchain, tx.script)
      script <- countVerifierComplexity(tx.script, blockchain, isAsset = true)
      _      <- checkEstimationOverflow(blockchain, script)
      snapshot <- StateSnapshot.build(
        blockchain,
        assetScripts = Map(asset -> script.map(AssetScriptInfo.tupled)),
        issuedAssets = VectorMap(asset -> NewAssetInfo(assetStatic, assetInfo, assetVolume)),
        portfolios = VectorMap(tx.sender.toAddress -> Portfolio.build(-tx.fee.value, asset, tx.quantity.value))
      )
    } yield snapshot
  }
}
