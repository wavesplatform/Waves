package com.wavesplatform.state2.diffs

import com.wavesplatform.features.Functionalities
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{AssetInfo, Diff, LeaseInfo, Portfolio}
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
        leaseInfo = LeaseInfo.empty,
        assets = Map(tx.assetId -> tx.quantity))),
      assetInfos = Map(tx.assetId -> info)))
  }

  def reissue(state: StateReader, fn: Functionalities, blockTime: Long, height: Int)(tx: ReissueTransaction): Either[ValidationError, Diff] = {
    findReferencedAsset(tx, state, tx.assetId).flatMap(_ => {
      val oldInfo = state.assetInfo(tx.assetId).get
      if (oldInfo.isReissuable || fn.allowInvalidReissueInSameBlockUpTo.check(blockTime).isRight) {
        Right(Diff(height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAddress -> Portfolio(
            balance = -tx.fee,
            leaseInfo = LeaseInfo.empty,
            assets = Map(tx.assetId -> tx.quantity))),
          assetInfos = Map(tx.assetId -> AssetInfo(
            volume = tx.quantity,
            isReissuable = tx.reissuable))))
      } else {
        Left(GenericError(s"Asset is not reissuable and invalid reissue is not allowed"))
      }
    })
  }

  def burn(state: StateReader, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
    findReferencedAsset(tx, state, tx.assetId).map(itx => {
      Diff(height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(
          balance = -tx.fee,
          leaseInfo = LeaseInfo.empty,
          assets = Map(tx.assetId -> -tx.amount))),
        assetInfos = Map(tx.assetId -> AssetInfo(isReissuable = true, volume = -tx.amount)))
    })
  }

  private def findReferencedAsset(tx: SignedTransaction, state: StateReader, assetId: AssetId): Either[ValidationError, IssueTransaction] = {
    state.findTransaction[IssueTransaction](assetId) match {
      case None => Left(GenericError("Referenced assetId not found"))
      case Some(itx) if !(itx.sender equals tx.sender) => Left(GenericError("Asset was issued by other address"))
      case Some(itx) => Right(itx)
    }
  }

}
