package com.wavesplatform.state.diffs

import cats.instances.option.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.ior.*
import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.ComplexityCheckPolicyProvider.*
import com.wavesplatform.features.EstimatorProvider.*
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.lang.v1.traits.domain.*
import com.wavesplatform.state.{AssetVolumeInfo, Blockchain, LeaseBalance, LeaseDetails, LeaseStaticInfo, Portfolio, SponsorshipValue, StateSnapshot}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxPositiveAmount
import com.wavesplatform.transaction.TxValidationError.GenericError

object DiffsCommon {
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

        val fixEstimateOfVerifier = blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
        def complexity(estimator: ScriptEstimator) =
          Script.verifierComplexity(
            script,
            estimator,
            fixEstimateOfVerifier,
            useContractVerifierLimit = !isAsset && blockchain.useReducedVerifierComplexityLimit
          )

        val cost =
          if (useV1PreCheck) complexity(ScriptEstimatorV1) *> complexity(ScriptEstimatorV2)
          else complexity(blockchain.estimator)

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
  ): Either[ValidationError, StateSnapshot] = {
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
            val portfolio  = Portfolio.build(-fee, asset, reissue.quantity)
            StateSnapshot.build(
              blockchain,
              portfolios = Map(sender -> portfolio),
              updatedAssets = Map(IssuedAsset(reissue.assetId) -> volumeInfo.rightIor)
            )
          }
        } else {
          Left(GenericError("Asset is not reissuable"))
        }
      }
  }

  def processBurn(blockchain: Blockchain, sender: Address, fee: Long, burn: Burn): Either[ValidationError, StateSnapshot] = {
    val burnAnyTokensEnabled = blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens)
    val asset                = IssuedAsset(burn.assetId)

    validateAsset(blockchain, asset, sender, !burnAnyTokensEnabled).flatMap { _ =>
      val volumeInfo = AssetVolumeInfo(isReissuable = true, volume = -burn.quantity)
      val portfolio  = Portfolio.build(-fee, asset, -burn.quantity)
      StateSnapshot.build(
        blockchain,
        portfolios = Map(sender -> portfolio),
        updatedAssets = Map(asset -> volumeInfo.rightIor)
      )
    }
  }

  def processSponsor(blockchain: Blockchain, sender: Address, fee: Long, sponsorFee: SponsorFee): Either[ValidationError, StateSnapshot] = {
    val asset = IssuedAsset(sponsorFee.assetId)
    validateAsset(blockchain, asset, sender, issuerOnly = true).flatMap { _ =>
      Either
        .cond(
          !blockchain.hasAssetScript(asset),
          StateSnapshot.build(
            blockchain,
            portfolios = Map(sender -> Portfolio(balance = -fee)),
            sponsorships = Map(asset -> SponsorshipValue(sponsorFee.minSponsoredAssetFee.getOrElse(0)))
          ),
          GenericError("Sponsorship smart assets is disabled.")
        )
        .flatten
    }
  }

  def processLease(
      blockchain: Blockchain,
      amount: TxPositiveAmount,
      sender: PublicKey,
      recipient: AddressOrAlias,
      fee: Long,
      leaseId: ByteStr,
      txId: ByteStr
  ): Either[ValidationError, StateSnapshot] = {
    val senderAddress = sender.toAddress
    for {
      recipientAddress <- blockchain.resolveAlias(recipient)
      _ <- Either.cond(
        recipientAddress != senderAddress,
        (),
        GenericError("Cannot lease to self")
      )
      _ <- Either.cond(
        blockchain.leaseDetails(leaseId).isEmpty,
        (),
        GenericError(s"Lease with id=$leaseId is already in the state")
      )
      leaseBalance    = blockchain.leaseBalance(senderAddress)
      senderBalance   = blockchain.balance(senderAddress, Waves)
      requiredBalance = if (blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)) amount.value + fee else amount.value
      _ <- Either.cond(
        senderBalance - leaseBalance.out >= requiredBalance,
        (),
        GenericError(s"Cannot lease more than own: Balance: $senderBalance, already leased: ${leaseBalance.out}")
      )
      portfolioDiff = Map(
        senderAddress    -> Portfolio(-fee, LeaseBalance(0, amount.value)),
        recipientAddress -> Portfolio(0, LeaseBalance(amount.value, 0))
      )
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = portfolioDiff,
        newLeases = Map(leaseId -> LeaseStaticInfo(sender, recipientAddress, amount, txId, blockchain.height))
      )
    } yield snapshot
  }

  def processLeaseCancel(
      blockchain: Blockchain,
      sender: PublicKey,
      fee: Long,
      time: Long,
      leaseId: ByteStr,
      cancelTxId: ByteStr
  ): Either[ValidationError, StateSnapshot] = {
    val allowedTs = blockchain.settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp
    for {
      lease <- blockchain.leaseDetails(leaseId).toRight(GenericError(s"Lease with id=$leaseId not found"))
      _ <- Either.cond(
        lease.isActive || time <= allowedTs,
        (),
        GenericError(s"Cannot cancel already cancelled lease")
      )
      _ <- Either.cond(
        sender == lease.sender || time < allowedTs,
        (),
        GenericError(
          s"LeaseTransaction was leased by other sender and " +
            s"time=$time > allowMultipleLeaseCancelTransactionUntilTimestamp=$allowedTs"
        )
      )
      senderPortfolio    = Map[Address, Portfolio](sender.toAddress -> Portfolio(-fee, LeaseBalance(0, -lease.amount.value)))
      recipientPortfolio = Map(lease.recipientAddress -> Portfolio(0, LeaseBalance(-lease.amount.value, 0)))
      portfolios <- Portfolio.combine(senderPortfolio, recipientPortfolio).leftMap(GenericError(_))
      snapshot <- StateSnapshot.build(
        blockchain,
        portfolios = portfolios,
        cancelledLeases = Map(leaseId -> LeaseDetails.Status.Cancelled(blockchain.height, Some(cancelTxId)))
      )
    } yield snapshot
  }
}
