package com.wavesplatform.state.diffs

import cats.data.Chain
import cats.syntax.foldable.*
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.EthereumTransaction.Transfer
import com.wavesplatform.transaction.TxValidationError.*
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.smart.*
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.validation.impl.DataTxValidator

object FeeValidation {

  case class FeeDetails(asset: Asset, requirements: Chain[String], minFeeInAsset: Long, minFeeInWaves: Long)

  val ScriptExtraFee    = 400000L
  val FeeUnit           = 100000
  val NFTMultiplier     = 0.001
  val BlockV5Multiplier = 0.001

  val FeeConstants: Map[TransactionType.TransactionType, Long] = Map(
    TransactionType.Genesis          -> 0,
    TransactionType.Payment          -> 1,
    TransactionType.Issue            -> 1000,
    TransactionType.Reissue          -> 1000,
    TransactionType.Burn             -> 1,
    TransactionType.Transfer         -> 1,
    TransactionType.MassTransfer     -> 1,
    TransactionType.Lease            -> 1,
    TransactionType.LeaseCancel      -> 1,
    TransactionType.Exchange         -> 3,
    TransactionType.CreateAlias      -> 1,
    TransactionType.Data             -> 1,
    TransactionType.SetScript        -> 10,
    TransactionType.SponsorFee       -> 1000,
    TransactionType.SetAssetScript   -> (1000 - 4),
    TransactionType.InvokeScript     -> 5,
    TransactionType.UpdateAssetInfo  -> 1,
    TransactionType.Ethereum         -> 1,
    TransactionType.InvokeExpression -> 10
  )

  def apply(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Unit] = {
    if (blockchain.height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain)) {
      for {
        feeDetails <- getMinFee(blockchain, tx)
        _ <- Either.cond(
          feeDetails.minFeeInAsset <= tx.fee,
          (),
          notEnoughFeeError(tx.tpe, feeDetails, tx.fee)
        )
      } yield ()
    } else {
      Either.cond(tx.fee > 0 || !tx.isInstanceOf[Authorized], (), GenericError(s"Fee must be positive."))
    }
  }

  private def notEnoughFeeError(txType: TransactionType.TransactionType, feeDetails: FeeDetails, feeAmount: Long): ValidationError = {
    val actualFee   = s"$feeAmount in ${feeDetails.asset.fold("WAVES")(_.id.toString)}"
    val requiredFee = s"${feeDetails.minFeeInWaves} WAVES${feeDetails.asset.fold("")(id => s" or ${feeDetails.minFeeInAsset} ${id.id.toString}")}"

    val errorMessage = s"Fee for ${txType.transactionName} ($actualFee) does not exceed minimal value of $requiredFee."

    GenericError((if (feeDetails.requirements.nonEmpty) (feeDetails.requirements mkString_ ". ") ++ ". " else "") ++ errorMessage)
  }

  private case class FeeInfo(assetInfo: Option[(IssuedAsset, AssetDescription)], requirements: Chain[String], wavesFee: Long)

  private def feeInUnits(blockchain: Blockchain, tx: Transaction): Either[ValidationError, Long] = {
    FeeConstants
      .get(tx.tpe)
      .map { baseFee =>
        tx match {
          case tx: MassTransferTransaction =>
            baseFee + (tx.transfers.size + 1) / 2
          case tx: DataTransaction =>
            val payloadLength =
              if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) DataTxValidator.realUserPayloadSize(tx.data)
              else if (tx.isProtobufVersion) tx.protoDataPayload.length
              else if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccounts)) tx.bodyBytes().length
              else tx.bytes().length

            baseFee + (payloadLength - 1) / 1024
          case itx: IssueTransaction =>
            val multiplier = if (blockchain.isNFT(itx)) NFTMultiplier else 1

            (baseFee * multiplier).toLong
          case _: ReissueTransaction =>
            val multiplier = if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)) BlockV5Multiplier else 1
            (baseFee * multiplier).toLong
          case _: SponsorFeeTransaction =>
            val multiplier = if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)) BlockV5Multiplier else 1
            (baseFee * multiplier).toLong
          case et: EthereumTransaction =>
            et.payload match {
              case _: EthereumTransaction.Transfer   => 1
              case _: EthereumTransaction.Invocation => 5
            }

          case ss: SetScriptTransaction if blockchain.isFeatureActivated(BlockchainFeatures.RideV6) =>
            ss.script.fold(1) { script =>
              val scriptSize = script.bytes().size
              val kbs        = scriptSize / 1024
              if (scriptSize > 0 && scriptSize % 1024 == 0) kbs else kbs + 1
            }

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
      case _: InvokeScriptTransaction =>
        if (blockchain.isFeatureActivated(BlockchainFeatures.SynchronousCalls)) 0 else tx.smartAssets(blockchain).size
      case tx: ExchangeTransaction =>
        tx.smartAssets(blockchain).size /* *3 if we decide to check orders and transaction */
      case EthereumTransaction(t: Transfer, _, _, _) =>
        t.tryResolveAsset(blockchain)
          .collectFirst { case i: IssuedAsset => blockchain.hasAssetScript(i) }
          .fold(0)(hasAsset => if (hasAsset) 1 else 0)
      case _ =>
        tx.smartAssets(blockchain).size
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
      case _: EthereumTransaction          => 0
      case tx: Transaction with Authorized => if (blockchain.hasPaidVerifier(tx.sender.toAddress)) 1 else 0
      case _                               => 0
    }

    val extraFee = smartAccountScriptsCount * ScriptExtraFee
    val extraRequirements =
      if (smartAccountScriptsCount > 0) Chain(s"Transaction sent from smart account. Requires $extraFee extra fee")
      else Chain.empty

    val FeeInfo(feeAssetInfo, reqs, feeAmount) = inputFee

    FeeInfo(feeAssetInfo, extraRequirements ++ reqs, feeAmount + extraFee)
  }

  def getMinFee(blockchain: Blockchain, tx: Transaction): Either[ValidationError, FeeDetails] = {
    feeAfterSponsorship(tx.feeAssetId, blockchain, tx)
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
