package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.lang.v1.traits.domain.{Burn, Reissue}
import com.wavesplatform.state.{AssetVolumeInfo, Blockchain, Diff, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.ProvenTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError

object DiffsCommon {
  def verifierComplexity(script: Script, estimator: ScriptEstimator): Either[String, Long] =
    Script
      .complexityInfo(script, estimator)
      .map(calcVerifierComplexity(script, _))

  private def calcVerifierComplexity(
      script: Script,
      complexity: (Long, Map[String, Long])
  ): Long = {
    val (totalComplexity, cm) = complexity
    script match {
      case ContractScriptImpl(_, DApp(_, _, _, Some(vf))) if cm.contains(vf.u.name) => cm(vf.u.name)
      case _                                                                        => totalComplexity
    }
  }

  def countScriptRuns(blockchain: Blockchain, tx: ProvenTransaction): Int =
    tx.checkedAssets.count(blockchain.hasAssetScript) + Some(tx.sender.toAddress).count(blockchain.hasAccountScript)

  def getScriptsComplexity(blockchain: Blockchain, tx: ProvenTransaction): Long = {
    val assetsComplexity = tx.checkedAssets.toList
      .flatMap(blockchain.assetScript)
      .map(_._2)

    val accountComplexity = blockchain
      .accountScript(tx.sender.toAddress)
      .map(_.maxComplexity)

    assetsComplexity.sum + accountComplexity.getOrElse(0L)
  }

  def countVerifierComplexity(
      script: Option[Script],
      blockchain: Blockchain
  ): Either[ValidationError, Option[(Script, Long)]] =
    script
      .traverse { script =>
        val useV1PreCheck =
          blockchain.height > blockchain.settings.functionalitySettings.estimatorPreCheckHeight &&
          !blockchain.isFeatureActivated(BlockchainFeatures.MultiPaymentInvokeScript)

        val cost =
          if (useV1PreCheck)
            Script.verifierComplexity(script, ScriptEstimatorV1) *>
            Script.verifierComplexity(script, ScriptEstimatorV2)
          else
            Script.verifierComplexity(script, blockchain.estimator)

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
}
