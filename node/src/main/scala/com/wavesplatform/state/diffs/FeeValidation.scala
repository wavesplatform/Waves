package com.wavesplatform.state.diffs

import cats.data.Chain
import cats.implicits._
import com.wavesplatform.features.BlockchainFeatures
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

  val ScriptExtraFee    = 400000L
  val FeeUnit           = 100000
  val NFTMultiplier     = 0.001
  val BlockV5Multiplier = 0.001

  val FeeConstants: Map[Byte, Long] = Map(
    GenesisTransaction.typeId         -> 0,
    PaymentTransaction.typeId         -> 1,
    IssueTransaction.typeId           -> 1000,
    ReissueTransaction.typeId         -> 1000,
    BurnTransaction.typeId            -> 1,
    TransferTransaction.typeId        -> 1,
    MassTransferTransaction.typeId    -> 1,
    LeaseTransaction.typeId           -> 1,
    LeaseCancelTransaction.typeId     -> 1,
    ExchangeTransaction.typeId        -> 3,
    CreateAliasTransaction.typeId     -> 1,
    DataTransaction.typeId            -> 1,
    SetScriptTransaction.typeId       -> 10,
    SponsorFeeTransaction.typeId      -> 1000,
    SetAssetScriptTransaction.typeId  -> (1000 - 4),
    InvokeScriptTransaction.typeId    -> 5,
    UpdateAssetInfoTransaction.typeId -> 1
  )

  def apply(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Unit] = {
    if (blockchain.height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)) {
      for {
        feeDetails <- getMinFee(blockchain, tx)
        _ <- Either.cond(
          feeDetails.minFeeInAsset <= tx.assetFee._2,
          (),
          notEnoughFeeError(tx.typeId, feeDetails, tx.assetFee._2)
        )
      } yield ()
    } else {
      Either.cond(tx.assetFee._2 > 0 || !tx.isInstanceOf[Authorized], (), GenericError(s"Fee must be positive."))
    }
  }

  private def notEnoughFeeError(txType: Byte, feeDetails: FeeDetails, feeAmount: Long): ValidationError = {
    val txName      = Constants.TransactionNames(txType)
    val actualFee   = s"$feeAmount in ${feeDetails.asset.fold("WAVES")(_.id.toString)}"
    val requiredFee = s"${feeDetails.minFeeInWaves} WAVES${feeDetails.asset.fold("")(id => s" or ${feeDetails.minFeeInAsset} ${id.id.toString}")}"

    val errorMessage = s"Fee for $txName ($actualFee) does not exceed minimal value of $requiredFee."

    GenericError((feeDetails.requirements mkString_ " ") ++ ". " ++ errorMessage)
  }

  private case class FeeInfo(assetInfo: Option[(IssuedAsset, AssetDescription)], requirements: Chain[String], wavesFee: Long)

  private def feeInUnits(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Long] = {
    FeeConstants
      .get(tx.typeId)
      .map { baseFee =>
        tx match {
          case tx: MassTransferTransaction =>
            baseFee + (tx.transfers.size + 1) / 2
          case tx: DataTransaction =>
            val payload =
              if (tx.isProtobufVersion) tx.protoDataPayload
              else if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts)) tx.bodyBytes()
              else tx.bytes()

            baseFee + (payload.length - 1) / 1024
          case itx: IssueTransaction =>
            val multiplier = if (blockchain.isNFT(itx)) NFTMultiplier else 1

            (baseFee * multiplier).toLong
          case _: ReissueTransaction =>
            val multiplier = if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)) BlockV5Multiplier else 1
            (baseFee * multiplier).toLong
          case _: SponsorFeeTransaction =>
            val multiplier = if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)) BlockV5Multiplier else 1
            (baseFee * multiplier).toLong
          case _ => baseFee
        }
      }
      .toRight(UnsupportedTransactionType)
  }

  private def feeAfterSponsorship(txAsset: Asset, blockchain: Blockchain, tx: Transaction): Either[ValidationError, FeeInfo] = {
    if (blockchain.height < Sponsorship.sponsoredFeesSwitchHeight(blockchain)) {
      // This could be true for private blockchains
      feeInUnits(blockchain, tx).map(x => FeeInfo(None, Chain.empty, x * FeeUnit))
    } else {
      for {
        feeInUnits <- feeInUnits(blockchain, tx)
        r <- txAsset match {
          case Waves => Right(FeeInfo(None, Chain.empty, feeInUnits * FeeUnit))
          case assetId @ IssuedAsset(_) =>
            for {
              assetInfo <- blockchain
                .assetDescription(assetId)
                .toRight(GenericError(s"Asset ${assetId.id.toString} does not exist, cannot be used to pay fees"))
              wavesFee <- Either.cond(
                assetInfo.sponsorship > 0,
                feeInUnits * FeeUnit,
                GenericError(s"Asset ${assetId.id.toString} is not sponsored, cannot be used to pay fees")
              )
            } yield FeeInfo(Some((assetId, assetInfo)), Chain.empty, wavesFee)
        }
      } yield r
    }
  }

  private def feeAfterSmartTokens(blockchain: Blockchain, tx: Transaction)(inputFee: FeeInfo): FeeInfo = {
    val FeeInfo(feeAssetInfo, requirements, feeAmount) = inputFee

    val tokenIsSmart: Boolean =
      feeAssetInfo
        .map(_._1)
        .flatMap(blockchain.assetDescription)
        .exists(_.script.isDefined)

    val assetsCount = tx match {
      case _: InvokeScriptTransaction if blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls) => 0
      case tx: ExchangeTransaction                                                                          => tx.checkedAssets.count(blockchain.hasAssetScript) /* *3 if we decide to check orders and transaction */
      case _                                                                                                => tx.checkedAssets.count(blockchain.hasAssetScript)
    }

    val finalAssetsCount =
      if (tokenIsSmart) assetsCount + 1
      else assetsCount

    val extraFee = finalAssetsCount * ScriptExtraFee

    val extraRequirements =
      if (finalAssetsCount > 0)
        Chain(s"Transaction involves $finalAssetsCount scripted assets. Requires $extraFee extra fee")
      else Chain.empty

    FeeInfo(feeAssetInfo, extraRequirements ++ requirements, feeAmount + extraFee)
  }

  private def feeAfterSmartAccounts(blockchain: Blockchain, tx: Transaction)(inputFee: FeeInfo): FeeInfo = {
    val smartAccountScriptsCount: Int = tx match {
      case tx: Transaction with Authorized => if (blockchain.hasPaidVerifier(tx.sender.toAddress)) 1 else 0
      case _                               => 0
    }

    val extraFee = smartAccountScriptsCount * ScriptExtraFee
    val extraRequeirements =
      if (smartAccountScriptsCount > 0) Chain(s"Transaction sent from smart account. Requires $extraFee extra fee.")
      else Chain.empty

    val FeeInfo(feeAssetInfo, reqs, feeAmount) = inputFee

    FeeInfo(feeAssetInfo, extraRequeirements ++ reqs, feeAmount + extraFee)
  }

  def getMinFee(blockchain: Blockchain, tx: Transaction): Either[ValidationError, FeeDetails] = {
    feeAfterSponsorship(tx.assetFee._1, blockchain, tx)
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
