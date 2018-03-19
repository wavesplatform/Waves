package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{AssetDescription, AssetInfo, Diff, LeaseBalance, Portfolio}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}
import scorex.transaction.{AssetId, SignedTransaction, ValidationError}

import scala.util.{Left, Right}

object AssetTransactionsDiff {

  def issue(height: Int)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    val info = AssetInfo(
      isReissuable = tx.reissuable,
      volume = tx.quantity)
    Right(Diff(height = height,
      tx = tx,
      portfolios = Map(tx.sender.toAddress -> Portfolio(
        balance = -tx.fee,
        lease = LeaseBalance.empty,
        assets = Map(tx.assetId() -> tx.quantity))),
      assetInfos = Map(tx.assetId() -> info)))
  }

  def reissue(state: SnapshotStateReader, settings: FunctionalitySettings, blockTime: Long, height: Int)(tx: ReissueTransaction): Either[ValidationError, Diff] =
    findReferencedAsset(tx, state, tx.assetId).flatMap { oldInfo =>
      if (oldInfo.reissuable || blockTime <= settings.allowInvalidReissueInSameBlockUntilTimestamp) {
        Right(Diff(height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(
            balance = -tx.fee,
            lease = LeaseBalance.empty,
            assets = Map(tx.assetId -> tx.quantity))),
          assetInfos = Map(tx.assetId -> AssetInfo(
            volume = tx.quantity,
            isReissuable = tx.reissuable))))
      } else {
        Left(GenericError(s"Asset is not reissuable and blockTime=$blockTime is greater than " +
          s"settings.allowInvalidReissueInSameBlockUntilTimestamp=${settings.allowInvalidReissueInSameBlockUntilTimestamp}"))
      }
    }

  def burn(state: SnapshotStateReader, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] =
    findReferencedAsset(tx, state, tx.assetId).map { _ =>
      Diff(height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(
          balance = -tx.fee,
          lease = LeaseBalance.empty,
          assets = Map(tx.assetId -> -tx.amount))),
        assetInfos = Map(tx.assetId -> AssetInfo(isReissuable = true, volume = -tx.amount)))
    }

  private def findReferencedAsset(tx: SignedTransaction, state: SnapshotStateReader, assetId: AssetId): Either[ValidationError, AssetDescription] = {
    state.assetDescription(assetId) match {
      case None => Left(GenericError("Referenced assetId not found"))
      case Some(ad) if !(ad.issuer equals tx.sender) => Left(GenericError("Asset was issued by another address"))
      case Some(itx) => Right(itx)
    }
  }
}
