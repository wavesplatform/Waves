package com.wavesplatform.state.diffs.modern

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{AssetInfo, Blockchain, Diff, LeaseBalance, Portfolio}
import scorex.account.{Address, PublicKeyAccount}
import scorex.transaction.AssetId
import scorex.transaction.modern.ModernTransaction
import scorex.transaction.modern.assets.{BurnTx, IssueTx, ReissueTx, TransferTx}
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.GenericError
import com.wavesplatform.features.FeatureProvider._
import cats.implicits._
import scala.util.{Left, Right}

object AssetTxDiff {
  def issue(height: Int)(tx: IssueTx): Either[ValidationError, Diff] = {
    val info = AssetInfo(isReissuable = tx.payload.reissuable, volume = tx.payload.quantity, script = None)
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(
          tx.sender.toAddress         -> Portfolio(balance = -tx.header.fee, lease = LeaseBalance.empty, assets = Map(tx.assetId() -> tx.payload.quantity))),
        assetInfos = Map(tx.assetId() -> info)
      ))
  }

  def reissue(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(tx: ReissueTx): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.payload.assetId, issuerOnly = true).flatMap { _ =>
      val oldInfo = blockchain.assetDescription(tx.payload.assetId).get
      if (!oldInfo.reissuable && blockTime > settings.allowInvalidReissueInSameBlockUntilTimestamp) {
        Left(
          GenericError(s"Asset is not reissuable and blockTime=$blockTime is greater than " +
            s"settings.allowInvalidReissueInSameBlockUntilTimestamp=${settings.allowInvalidReissueInSameBlockUntilTimestamp}"))
      } else if ((Long.MaxValue - tx.payload.quantity) < oldInfo.totalVolume && blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens,
                                                                                                              blockchain.height)) {
        Left(GenericError(s"Asset total value overflow"))
      } else {
        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios = Map(
              tx.sender.toAddress -> Portfolio(balance = -tx.header.fee,
                                               lease = LeaseBalance.empty,
                                               assets = Map(tx.payload.assetId -> tx.payload.quantity))),
            assetInfos = Map(tx.payload.assetId -> AssetInfo(volume = tx.payload.quantity, isReissuable = tx.payload.reissuable, script = None))
          ))
      }
    }

  def burn(blockchain: Blockchain, height: Int)(tx: BurnTx): Either[ValidationError, Diff] = {
    val burnAnyTokensEnabled = blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens, blockchain.height)

    validateAsset(tx, blockchain, tx.payload.assetId, !burnAnyTokensEnabled).map(itx => {
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(
          tx.sender.toAddress -> Portfolio(balance = -tx.header.fee,
                                           lease = LeaseBalance.empty,
                                           assets = Map(tx.payload.assetId -> -tx.payload.amount))),
        assetInfos = Map(tx.payload.assetId -> AssetInfo(isReissuable = true, volume = -tx.payload.amount, None))
      )
    })
  }

  def transfer(blockchain: Blockchain, height: Int)(tx: TransferTx): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.header.sender.publicKey)
    for {
      recipient <- blockchain.resolveAliasEi(tx.payload.recipient)
      portfolios = (tx.payload.assetId match {
        case None =>
          Map(sender -> Portfolio(-tx.payload.amount, LeaseBalance.empty, Map.empty)).combine(
            Map(recipient -> Portfolio(tx.payload.amount, LeaseBalance.empty, Map.empty))
          )
        case Some(aid) =>
          Map(sender -> Portfolio(0, LeaseBalance.empty, Map(aid -> -tx.payload.amount))).combine(
            Map(recipient -> Portfolio(0, LeaseBalance.empty, Map(aid -> tx.payload.amount)))
          )
      }).combine(Map(sender -> Portfolio(-tx.header.fee, LeaseBalance.empty, Map.empty)))
      assetIssued = tx.payload.assetId match {
        case None      => true
        case Some(aid) => blockchain.assetDescription(aid).isDefined
      }
      _ <- Either.cond(assetIssued, (), GenericError(s"Unissued assets are not allowed"))
    } yield Diff(height, tx, portfolios)
  }

  private def validateAsset(tx: ModernTransaction, blockchain: Blockchain, assetId: AssetId, issuerOnly: Boolean): Either[ValidationError, Unit] = {
    blockchain.transactionInfo(assetId) match {
      case Some((_, itx: IssueTx)) if itx.payload.script.isEmpty && !validIssuer(issuerOnly, tx.sender, itx.sender) =>
        Left(GenericError("Asset was issued by other address"))
      case Some(_) =>
        Right({})
      case None =>
        Left(GenericError("Referenced assetId not found"))
    }
  }

  private def validIssuer(issuerOnly: Boolean, sender: PublicKeyAccount, issuer: PublicKeyAccount): Boolean = {
    if (issuerOnly) sender equals issuer
    else true
  }
}
