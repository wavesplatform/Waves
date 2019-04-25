package com.wavesplatform.state.diffs

import com.wavesplatform.account.PublicKey
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{AssetInfo, Blockchain, Diff, LeaseBalance, Portfolio, SponsorshipValue}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.ProvenTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets._

import scala.util.{Left, Right}

object AssetTransactionsDiff {
  def issue(blockchain: Blockchain, height: Int)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    val info  = AssetInfo(isReissuable = tx.reissuable, volume = tx.quantity)
    val asset = IssuedAsset(tx.id())
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(asset -> tx.quantity))),
        assetInfos = Map(asset               -> info),
        assetScripts = Map(asset -> tx.script).filter(_._2.isDefined),
        scriptsRun = countScriptRuns(blockchain, tx)
      ))
  }

  def setAssetScript(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: SetAssetScriptTransaction): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      if (blockchain.hasAssetScript(tx.asset)) {
        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
            assetScripts = Map(tx.asset          -> tx.script),
            scriptsRun = Some(tx.sender.toAddress).count(blockchain.hasScript) // scripts(blockchain, tx) // TODO: Consider fixing with fork parameter
          ))
      } else {
        Left(GenericError("Cannot set script on an asset issued without a script"))
      }
    }

  def reissue(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: ReissueTransaction): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      val oldInfo = blockchain.assetDescription(tx.asset).get

      def wasBurnt: Boolean = {
        val burns = blockchain
          .addressTransactions(tx.sender, Set(BurnTransaction.typeId), Int.MaxValue, None)
          .getOrElse(Nil)

        val result = burns.collectFirst { case (_, btx: BurnTransaction) if btx.asset == btx.asset => btx }
        result.isDefined
      }

      val isDataTxActivated = blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, blockchain.height)
      if (oldInfo.reissuable || (blockTime <= settings.allowInvalidReissueInSameBlockUntilTimestamp) || (!isDataTxActivated && wasBurnt)) {
        if ((Long.MaxValue - tx.quantity) < oldInfo.totalVolume && isDataTxActivated) {
          Left(GenericError("Asset total value overflow"))
        } else {
          Right(
            Diff(
              height = height,
              tx = tx,
              portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.asset -> tx.quantity))),
              assetInfos = Map(tx.asset            -> AssetInfo(volume = tx.quantity, isReissuable = tx.reissuable)),
              scriptsRun = countScriptRuns(blockchain, tx)
            ))
        }
      } else {
        Left(GenericError("Asset is not reissuable"))
      }
    }

  def burn(blockchain: Blockchain, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
    val burnAnyTokensEnabled = blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens, blockchain.height)

    validateAsset(tx, blockchain, tx.asset, !burnAnyTokensEnabled).map { _ =>
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.asset -> -tx.quantity))),
        assetInfos = Map(tx.asset            -> AssetInfo(isReissuable = true, volume = -tx.quantity)),
        scriptsRun = countScriptRuns(blockchain, tx)
      )
    }
  }

  def sponsor(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: SponsorFeeTransaction): Either[ValidationError, Diff] = {
    validateAsset(tx, blockchain, tx.asset, issuerOnly = true).flatMap { _ =>
      Either.cond(
        !blockchain.hasAssetScript(tx.asset),
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
          sponsorship = Map(tx.asset           -> SponsorshipValue(tx.minSponsoredAssetFee.getOrElse(0))),
          scriptsRun = countScriptRuns(blockchain, tx)
        ),
        GenericError("Sponsorship smart assets is disabled.")
      )
    }
  }

  private[this] def validateAsset(tx: ProvenTransaction,
                                  blockchain: Blockchain,
                                  assetId: IssuedAsset,
                                  issuerOnly: Boolean): Either[ValidationError, Unit] = {
    @inline
    def validIssuer(issuerOnly: Boolean, sender: PublicKey, issuer: PublicKey) =
      !issuerOnly || sender == issuer

    blockchain.transactionInfo(assetId.id) match {
      case Some((_, sitx: IssueTransaction)) if !validIssuer(issuerOnly, tx.sender, sitx.sender) =>
        Left(GenericError("Asset was issued by other address"))
      case None =>
        Left(GenericError("Referenced assetId not found"))
      case Some(_) =>
        Right({})
    }
  }

  private[this] def countScriptRuns(blockchain: Blockchain, tx: ProvenTransaction): Int =
    tx.checkedAssets().count(blockchain.hasAssetScript) + Some(tx.sender.toAddress).count(blockchain.hasScript)
}
