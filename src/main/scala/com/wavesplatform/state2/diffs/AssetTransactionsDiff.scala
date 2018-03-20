package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{AssetInfo, Diff, LeaseInfo, Portfolio}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, SmartIssueTransaction}
import scorex.transaction.{AssetId, SignedTransaction, ValidationError}

import scala.util.{Left, Right}

object AssetTransactionsDiff {

  def issue(height: Int)(tx: IssueTransaction): Either[ValidationError, Diff] = {
    val info = AssetInfo(isReissuable = tx.reissuable, volume = tx.quantity, script = None)
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, leaseInfo = LeaseInfo.empty, assets = Map(tx.assetId() -> tx.quantity))),
        assetInfos = Map(tx.assetId()        -> info)
      ))
  }

  def smartIssue(height: Int)(tx: SmartIssueTransaction): Either[ValidationError, Diff] = {
    val info = AssetInfo(isReissuable = tx.reissuable, volume = tx.quantity, script = tx.script)
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, leaseInfo = LeaseInfo.empty, assets = Map(tx.id() -> tx.quantity))),
        assetInfos = Map(tx.id()             -> info)
      ))
  }

  def reissue(state: SnapshotStateReader, settings: FunctionalitySettings, blockTime: Long, height: Int)(
      tx: ReissueTransaction): Either[ValidationError, Diff] = {
    validateAsset(tx, state, tx.assetId).flatMap(_ => {
      val oldInfo = state.assetInfo(tx.assetId).get
      if (oldInfo.isReissuable || blockTime <= settings.allowInvalidReissueInSameBlockUntilTimestamp) {
        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios =
              Map(tx.sender.toAddress   -> Portfolio(balance = -tx.fee, leaseInfo = LeaseInfo.empty, assets = Map(tx.assetId -> tx.quantity))),
            assetInfos = Map(tx.assetId -> AssetInfo(volume = tx.quantity, isReissuable = tx.reissuable, script = None))
          ))
      } else {
        Left(
          GenericError(s"Asset is not reissuable and blockTime=$blockTime is greater than " +
            s"settings.allowInvalidReissueInSameBlockUntilTimestamp=${settings.allowInvalidReissueInSameBlockUntilTimestamp}"))
      }
    })
  }

  def burn(state: SnapshotStateReader, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
    validateAsset(tx, state, tx.assetId).map(_ => {
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(balance = -tx.fee, leaseInfo = LeaseInfo.empty, assets = Map(tx.assetId -> -tx.amount))),
        assetInfos = Map(tx.assetId          -> AssetInfo(isReissuable = true, volume = -tx.amount, None))
      )
    })
  }

  private def validateAsset(tx: SignedTransaction, state: SnapshotStateReader, assetId: AssetId): Either[ValidationError, Unit] = {
    state.findTransaction[IssueTransaction](assetId).orElse(state.findTransaction[SmartIssueTransaction](assetId)) match {
      case None                                                                       => Left(GenericError("Referenced assetId not found"))
      case Some(SmartIssueTransaction(_, _, _, _, _, _, _, _, Some(script), _, _, _)) => Right({})
      case Some(itx) if !(itx.sender equals tx.sender)                                => Left(GenericError("Asset was issued by other address"))
      case Some(_)                                                                    => Right({})
    }
  }
}
