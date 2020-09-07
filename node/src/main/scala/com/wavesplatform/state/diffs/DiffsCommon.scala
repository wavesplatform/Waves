package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.ComplexityCheckPolicyProvider._
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.traits.domain.{Burn, Reissue, SponsorFee}
import com.wavesplatform.state.{AssetVolumeInfo, Blockchain, Diff, LeaseBalance, Portfolio, SponsorshipValue}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{ProvenTransaction, Transaction}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction

object DiffsCommon {
  def countScriptRuns(blockchain: Blockchain, tx: ProvenTransaction): Int =
    tx.checkedAssets.count(blockchain.hasAssetScript) + Some(tx.sender.toAddress).count(blockchain.hasAccountScript)

  def getAssetsComplexity(blockchain: Blockchain, tx: Transaction): Long =
    tx match {
      case ptx: ProvenTransaction =>
        ptx.checkedAssets.toList
          .flatMap(blockchain.assetScript)
          .map(_.complexity)
          .sum
      case _ => 0L
    }

  def getAccountsComplexity(blockchain: Blockchain, tx: Transaction): Long =
    (tx match {
      case etx: ExchangeTransaction => Seq(etx.sender, etx.buyOrder.senderPublicKey, etx.sellOrder.senderPublicKey)
      case ptx: ProvenTransaction   => Seq(ptx.sender)
      case _                        => Seq.empty
    }).flatMap(pk => blockchain.accountScript(pk.toAddress).map(_.verifierComplexity)).sum

  def countVerifierComplexity(
      script: Option[Script],
      blockchain: Blockchain,
      isAsset: Boolean
  ): Either[ValidationError, Option[(Script, Long)]] =
    script
      .traverse { script =>
        val useV1PreCheck =
          blockchain.height > blockchain.settings.functionalitySettings.estimatorPreCheckHeight &&
            !blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)

        val cost =
          if (useV1PreCheck)
            Script.verifierComplexity(script, ScriptEstimatorV1, !isAsset && blockchain.useReducedVerifierComplexityLimit) *>
              Script.verifierComplexity(script, ScriptEstimatorV2, !isAsset && blockchain.useReducedVerifierComplexityLimit)
          else
            Script.verifierComplexity(script, blockchain.estimator, !isAsset && blockchain.useReducedVerifierComplexityLimit)

        cost.map((script, _))
      }
      .leftMap(GenericError(_))

  def validateAsset(
      blockchain: Blockchain,
      asset: IssuedAsset,
      sender: Address,
      issuerOnly: Boolean
  ): Either[ValidationError, Unit] = {
    @inline
    def validIssuer(issuerOnly: Boolean, sender: Address, issuer: Address) =
      !issuerOnly || sender == issuer

    blockchain.assetDescription(asset) match {
      case Some(ad) if !validIssuer(issuerOnly, sender, ad.issuer.toAddress) =>
        Left(GenericError("Asset was issued by other address"))
      case None =>
        Left(GenericError("Referenced assetId not found"))
      case Some(_) =>
        Right({})
    }
  }

  def processReissue(
      blockchain: Blockchain,
      sender: Address,
      blockTime: Long,
      fee: Long,
      reissue: Reissue
  ): Either[ValidationError, Diff] = {
    val asset = IssuedAsset(reissue.assetId)
    validateAsset(blockchain, asset, sender, issuerOnly = true)
      .flatMap { _ =>
        val oldInfo = blockchain.assetDescription(asset).get

        val isDataTxActivated = blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, blockchain.height)
        if (oldInfo.reissuable || (blockTime <= blockchain.settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp)) {
          if ((Long.MaxValue - reissue.quantity) < oldInfo.totalVolume && isDataTxActivated) {
            Left(GenericError("Asset total value overflow"))
          } else {
            val volumeInfo = AssetVolumeInfo(reissue.isReissuable, BigInt(reissue.quantity))
            val portfolio  = Portfolio(balance = -fee, lease = LeaseBalance.empty, assets = Map(asset -> reissue.quantity))

            Right(
              Diff.stateOps(
                portfolios = Map(sender                          -> portfolio),
                updatedAssets = Map(IssuedAsset(reissue.assetId) -> volumeInfo.rightIor)
              )
            )
          }
        } else {
          Left(GenericError("Asset is not reissuable"))
        }
      }
  }

  def processBurn(blockchain: Blockchain, sender: Address, fee: Long, burn: Burn): Either[ValidationError, Diff] = {
    val burnAnyTokensEnabled = blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens)
    val asset                = IssuedAsset(burn.assetId)

    validateAsset(blockchain, asset, sender, !burnAnyTokensEnabled).map { _ =>
      val volumeInfo = AssetVolumeInfo(isReissuable = true, volume = -burn.quantity)
      val portfolio  = Portfolio(balance = -fee, lease = LeaseBalance.empty, assets = Map(asset -> -burn.quantity))

      Diff.stateOps(
        portfolios = Map(sender   -> portfolio),
        updatedAssets = Map(asset -> volumeInfo.rightIor)
      )
    }
  }

  def processSponsor(blockchain: Blockchain, sender: Address, fee: Long, sponsorFee: SponsorFee): Either[ValidationError, Diff] = {
    val asset = IssuedAsset(sponsorFee.assetId)
    DiffsCommon.validateAsset(blockchain, asset, sender, issuerOnly = true).flatMap { _ =>
      Either.cond(
        !blockchain.hasAssetScript(asset),
        Diff.stateOps(
          portfolios = Map(sender -> Portfolio(balance = -fee)),
          sponsorship = Map(asset -> SponsorshipValue(sponsorFee.minSponsoredAssetFee.getOrElse(0)))
        ),
        GenericError("Sponsorship smart assets is disabled.")
      )
    }
  }
}
