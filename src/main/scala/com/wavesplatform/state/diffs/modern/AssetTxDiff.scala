package com.wavesplatform.state.diffs.modern

import cats.implicits._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{AssetInfo, Blockchain, Diff, LeaseBalance, Portfolio, Sponsorship, SponsorshipValue}
import scorex.account.{Address, PublicKeyAccount}
import scorex.transaction.AssetId
import scorex.transaction.base._
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.GenericError

import scala.util.{Left, Right}

object AssetTxDiff {
  def issue(height: Int)(tx: IssueTxBase): Either[ValidationError, Diff] = {
    val info = AssetInfo(isReissuable = tx.reissuable, volume = tx.quantity, script = tx.script)
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.assetId() -> tx.quantity))),
        assetInfos = Map(tx.assetId()        -> info)
      ))
  }

  def reissue(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: ReissueTxBase): Either[ValidationError, Diff] =
    validateAsset(tx, blockchain, tx.assetId, issuerOnly = true).flatMap { _ =>
      val oldInfo = blockchain.assetDescription(tx.assetId).get
      if (!oldInfo.reissuable && blockTime > settings.allowInvalidReissueInSameBlockUntilTimestamp) {
        Left(
          GenericError(s"Asset is not reissuable and blockTime=$blockTime is greater than " +
            s"settings.allowInvalidReissueInSameBlockUntilTimestamp=${settings.allowInvalidReissueInSameBlockUntilTimestamp}"))
      } else if ((Long.MaxValue - tx.quantity) < oldInfo.totalVolume && blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens,
                                                                                                      blockchain.height)) {
        Left(GenericError(s"Asset total value overflow"))
      } else {
        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.assetId -> tx.quantity))),
            assetInfos = Map(tx.assetId          -> AssetInfo(volume = tx.quantity, isReissuable = tx.reissuable, script = None))
          ))
      }
    }

  def burn(blockchain: Blockchain, height: Int)(tx: BurnTxBase): Either[ValidationError, Diff] = {
    val burnAnyTokensEnabled = blockchain.isFeatureActivated(BlockchainFeatures.BurnAnyTokens, blockchain.height)

    validateAsset(tx, blockchain, tx.assetId, !burnAnyTokensEnabled).map(itx => {
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map(tx.assetId -> -tx.amount))),
        assetInfos = Map(tx.assetId          -> AssetInfo(isReissuable = true, volume = -tx.amount, None))
      )
    })
  }

  def transfer(blockchain: Blockchain, s: FunctionalitySettings, blockTime: Long, height: Int)
              (tx: TransferTxBase): Either[ValidationError, Diff] = {
    val sender = Address.fromPublicKey(tx.sender.publicKey)

    val isInvalidEi = for {
      recipient <- blockchain.resolveAliasEi(tx.recipient)
      _ <- Either.cond((tx.feeAssetId >>= blockchain.assetDescription >>= (_.script)).isEmpty,
        (),
        GenericError("Smart assets can't participate in TransferTransactions as a fee"))
      portfolios = (tx.assetId match {
        case None =>
          Map(sender -> Portfolio(-tx.amount, LeaseBalance.empty, Map.empty)).combine(
            Map(recipient -> Portfolio(tx.amount, LeaseBalance.empty, Map.empty))
          )
        case Some(aid) =>
          Map(sender -> Portfolio(0, LeaseBalance.empty, Map(aid -> -tx.amount))).combine(
            Map(recipient -> Portfolio(0, LeaseBalance.empty, Map(aid -> tx.amount)))
          )
      }).combine(
        tx.feeAssetId match {
          case None => Map(sender -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty))
          case Some(aid) =>
            val senderPf = Map(sender -> Portfolio(0, LeaseBalance.empty, Map(aid -> -tx.fee)))
            if (height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain, s)) {
              val sponsorPf = blockchain
                .assetDescription(aid)
                .collect {
                  case desc if desc.sponsorship > 0 =>
                    val feeInWaves = Sponsorship.toWaves(tx.fee, desc.sponsorship)
                    Map(desc.issuer.toAddress -> Portfolio(-feeInWaves, LeaseBalance.empty, Map(aid -> tx.fee)))
                }
                .getOrElse(Map.empty)
              senderPf.combine(sponsorPf)
            } else senderPf
        }
      )
      assetIssued    = tx.assetId.forall(blockchain.assetDescription(_).isDefined)
      feeAssetIssued = tx.feeAssetId.forall(blockchain.assetDescription(_).isDefined)
    } yield (portfolios, blockTime > s.allowUnissuedAssetsUntil && !(assetIssued && feeAssetIssued))

    isInvalidEi match {
      case Left(e) => Left(e)
      case Right((portfolios, invalid)) =>
        if (invalid)
          Left(GenericError(s"Unissued assets are not allowed after allowUnissuedAssetsUntil=${s.allowUnissuedAssetsUntil}"))
        else
          Right(Diff(height, tx, portfolios))
    }
  }

  def sponsor(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(tx: SponsorFeeTxBase): Either[ValidationError, Diff] = {
    validateAsset(tx, blockchain, tx.assetId, true).flatMap { _ =>
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
          sponsorship = Map(tx.assetId         -> SponsorshipValue(tx.minFee))
        ))
    }
  }

  def cancelSponsorship(blockchain: Blockchain, settings: FunctionalitySettings, blockTime: Long, height: Int)(tx: CancelFeeSponsorshipTxBase): Either[ValidationError, Diff] = {
    validateAsset(tx, blockchain, tx.assetId, true).flatMap { _ =>
      Right(
        Diff(
          height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, lease = LeaseBalance.empty, assets = Map.empty)),
          sponsorship = Map(tx.assetId         -> SponsorshipValue(0))
        ))
    }
  }

  private def validateAsset(tx: TxBase, blockchain: Blockchain, assetId: AssetId, issuerOnly: Boolean): Either[ValidationError, Unit] = {
    blockchain.transactionInfo(assetId) match {
      case Some((_, itx: IssueTxBase)) if itx.script.isEmpty && !validIssuer(issuerOnly, tx.sender, itx.sender) =>
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
