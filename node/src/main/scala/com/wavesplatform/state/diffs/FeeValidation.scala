package com.wavesplatform.state.diffs

import cats.data.Chain
import cats.implicits._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.Constants
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart._
import com.wavesplatform.transaction.transfer._

object FeeValidation {

  case class FeeDetails(asset: Asset, requirements: Chain[String], minFeeInAsset: Long, minFeeInWaves: Long)

  val ScriptExtraFee = 400000L
  val FeeUnit        = 100000
  val NFTMultiplier  = 0.001

  val FeeConstants: Map[Byte, Long] = Map(
    GenesisTransaction.typeId        -> 0,
    PaymentTransaction.typeId        -> 1,
    IssueTransaction.typeId          -> 1000,
    ReissueTransaction.typeId        -> 1000,
    BurnTransaction.typeId           -> 1,
    TransferTransaction.typeId       -> 1,
    MassTransferTransaction.typeId   -> 1,
    LeaseTransaction.typeId          -> 1,
    LeaseCancelTransaction.typeId    -> 1,
    ExchangeTransaction.typeId       -> 3,
    CreateAliasTransaction.typeId    -> 1,
    DataTransaction.typeId           -> 1,
    SetScriptTransaction.typeId      -> 10,
    SponsorFeeTransaction.typeId     -> 1000,
    SetAssetScriptTransaction.typeId -> (1000 - 4),
    InvokeScriptTransaction.typeId   -> 5
  )

  def apply(blockchain: Blockchain, height: Int, tx: Transaction): Either[ValidationError, Unit] = {
    if (height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)) {
      for {
        feeDetails <- getMinFee(blockchain, height, tx)
        minWaves   = feeDetails.minFeeInWaves
        minFee     = feeDetails.minFeeInAsset
        feeAssetId = feeDetails.asset
        _ <- Either.cond(
          minFee <= tx.assetFee._2,
          (),
          notEnoughFeeError(tx.builder.typeId, feeDetails, tx.assetFee._2)
        )
      } yield ()
    } else {
      Either.cond(tx.assetFee._2 > 0 || !tx.isInstanceOf[Authorized], (), GenericError(s"Fee must be positive."))
    }
  }

  private def notEnoughFeeError(txType: Byte, feeDetails: FeeDetails, feeAmount: Long): ValidationError = {
    val txName      = Constants.TransactionNames(txType)
    val actualFee   = s"$feeAmount in ${feeDetails.asset.fold("WAVES")(_.id.base58)}"
    val requiredFee = s"${feeDetails.minFeeInWaves} WAVES${feeDetails.asset.fold("")(id => s" or ${feeDetails.minFeeInAsset} ${id.id.base58}")}"

    val errorMessage = s"Fee for $txName ($actualFee) does not exceed minimal value of $requiredFee."

    GenericError((feeDetails.requirements mkString_ " ") ++ ". " ++ errorMessage)
  }

  private case class FeeInfo(assetInfo: Option[(IssuedAsset, AssetDescription)], requirements: Chain[String], wavesFee: Long)

  private def feeInUnits(blockchain: Blockchain, height: Int, tx: Transaction): Either[ValidationError, Long] = {
    FeeConstants
      .get(tx.builder.typeId)
      .map { baseFee =>
        tx match {
          case tx: MassTransferTransaction =>
            baseFee + (tx.transfers.size + 1) / 2
          case tx: DataTransaction =>
            val base = if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts, height)) tx.bodyBytes() else tx.bytes()
            baseFee + (base.length - 1) / 1024
          case itx: IssueTransaction =>
            lazy val nftActivated = blockchain.activatedFeatures
              .get(BlockchainFeatures.ReduceNFTFee.id)
              .exists(_ <= height)

            val multiplier = if (itx.isNFT && nftActivated) NFTMultiplier else 1

            (baseFee * multiplier).toLong
          case _ => baseFee
        }
      }
      .toRight(UnsupportedTransactionType)
  }

  private def feeAfterSponsorship(txAsset: Asset, height: Int, blockchain: Blockchain, tx: Transaction): Either[ValidationError, FeeInfo] = {
    if (height < Sponsorship.sponsoredFeesSwitchHeight(blockchain)) {
      // This could be true for private blockchains
      feeInUnits(blockchain, height, tx).map(x => FeeInfo(None, Chain.empty, x * FeeUnit))
    } else {
      for {
        feeInUnits <- feeInUnits(blockchain, height, tx)
        r <- txAsset match {
          case Waves => Right(FeeInfo(None, Chain.empty, feeInUnits * FeeUnit))
          case assetId @ IssuedAsset(_) =>
            for {
              assetInfo <- blockchain
                .assetDescription(assetId)
                .toRight(GenericError(s"Asset ${assetId.id.base58} does not exist, cannot be used to pay fees"))
              wavesFee <- Either.cond(
                assetInfo.sponsorship > 0,
                feeInUnits * FeeUnit,
                GenericError(s"Asset ${assetId.id.base58} is not sponsored, cannot be used to pay fees")
              )
            } yield FeeInfo(Some((assetId, assetInfo)), Chain.empty, wavesFee)
        }
      } yield r
    }
  }

  private def feeAfterSmartTokens(blockchain: Blockchain, tx: Transaction)(inputFee: FeeInfo): FeeInfo = {
    val FeeInfo(feeAssetInfo, reqirements, feeAmount) = inputFee

    val tokenIsSmart: Boolean =
      feeAssetInfo
        .map(_._1)
        .flatMap(blockchain.assetDescription)
        .exists(_.script.isDefined)

    val assetsCount = tx match {
      case tx: ExchangeTransaction => tx.checkedAssets().count(blockchain.hasAssetScript) /* *3 if we deside to check orders and transaction */
      case _                       => tx.checkedAssets().count(blockchain.hasAssetScript)
    }

    val finalAssetsCount =
      if (tokenIsSmart) assetsCount + 1
      else assetsCount

    val extraFee = finalAssetsCount * ScriptExtraFee

    val extraRequeirements =
      if (finalAssetsCount > 0)
        Chain(s"Transaction involves $finalAssetsCount scripted assets. Requires $extraFee extra fee")
      else Chain.empty

    FeeInfo(feeAssetInfo, extraRequeirements ++ reqirements, feeAmount + extraFee)
  }

  private def feeAfterSmartAccounts(blockchain: Blockchain, tx: Transaction)(inputFee: FeeInfo): FeeInfo = {
    val smartAccountScriptsCount: Int = tx match {
      case tx: Transaction with Authorized => if (blockchain.hasScript(tx.sender)) 1 else 0
      case _                               => 0
    }

    val extraFee = smartAccountScriptsCount * ScriptExtraFee
    val extraRequeirements =
      if (smartAccountScriptsCount > 0) Chain(s"Transaction sent from smart account. Requires $extraFee extra fee.")
      else Chain.empty

    val FeeInfo(feeAssetInfo, reqs, feeAmount) = inputFee

    FeeInfo(feeAssetInfo, extraRequeirements ++ reqs, feeAmount + extraFee)
  }

  def getMinFee(blockchain: Blockchain, height: Int, tx: Transaction): Either[ValidationError, FeeDetails] = {
    feeAfterSponsorship(tx.assetFee._1, height, blockchain, tx)
      .map(feeAfterSmartTokens(blockchain, tx))
      .map(feeAfterSmartAccounts(blockchain, tx))
      .map {
        case FeeInfo(Some((assetId, assetInfo)), reqs, amountInWaves) =>
          FeeDetails(assetId, reqs, Sponsorship.fromWaves(amountInWaves, assetInfo.sponsorship), amountInWaves)
        case FeeInfo(None, reqs, amountInWaves) =>
          FeeDetails(Waves, reqs, amountInWaves, amountInWaves)
      }
  }
}
