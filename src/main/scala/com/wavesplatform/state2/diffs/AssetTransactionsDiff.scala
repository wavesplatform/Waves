package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{AssetInfo, Diff, EqByteArray, Portfolio}
import scorex.account.Account
import scorex.transaction.StateValidationError
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}

import scala.util.{Left, Right}

object AssetTransactionsDiff {

  def issue(state: StateReader, height: Int)(tx: IssueTransaction): Either[StateValidationError, Diff] = {
    val assetId = EqByteArray(tx.assetId)
    val info = AssetInfo(
      isReissuable = tx.reissuable,
      volume = tx.quantity)
    Right(Diff(height = height,
      tx = tx,
      portfolios = Map(Account.fromPublicKey(tx.sender.publicKey) -> Portfolio(
        balance = -tx.fee,
        effectiveBalance = -tx.fee,
        assets = Map(assetId -> tx.quantity))),
      assetInfos = Map(assetId -> info)))
  }

  def reissue(state: StateReader, settings: FunctionalitySettings, height: Int, blockTime: Long)(tx: ReissueTransaction): Either[StateValidationError, Diff] = {
    val issueTxEi = state.findTransaction[IssueTransaction](tx.assetId) match {
      case None => Left(TransactionValidationError(tx, "Referenced assetId not found"))
      case Some(itx) if !(itx.sender equals tx.sender) => Left(TransactionValidationError(tx, "Asset was issued by other address"))
      case Some(itx) => Right(itx)
    }
    issueTxEi.flatMap(itx => {
      val assetId = EqByteArray(tx.assetId)
      val oldInfo = state.assetInfo(assetId).get
      if (oldInfo.isReissuable || blockTime <= settings.allowInvalidReissueInSameBlockUntilTimestamp) {
        Right(Diff(height = height,
          tx = tx,
          portfolios = Map(Account.fromPublicKey(tx.sender.publicKey) -> Portfolio(
            balance = -tx.fee,
            effectiveBalance = -tx.fee,
            assets = Map(assetId -> tx.quantity))),
          assetInfos = Map(assetId -> AssetInfo(
            volume = tx.quantity,
            isReissuable = tx.reissuable))))
      } else {
        Left(TransactionValidationError(tx, s"Asset is not reissuable and blockTime=$blockTime is greater than " +
          s"settings.allowInvalidReissueInSameBlockUntilTimestamp=${settings.allowInvalidReissueInSameBlockUntilTimestamp}"))
      }
    })
  }

  def burn(state: StateReader, height: Int)(tx: BurnTransaction): Either[StateValidationError, Diff] = {
    val issueTxEi = state.findTransaction[IssueTransaction](tx.assetId) match {
      case None => Left(TransactionValidationError(tx, "Referenced assetId not found"))
      case Some(itx) if !(itx.sender equals tx.sender) => Left(TransactionValidationError(tx, "Asset was issued by other address"))
      case Some(itx) => Right(itx)
    }
    issueTxEi.map(itx => {
      val assetId = EqByteArray(tx.assetId)
      Diff(height = height,
        tx = tx,
        portfolios = Map(Account.fromPublicKey(tx.sender.publicKey) -> Portfolio(
          balance = -tx.fee,
          effectiveBalance = -tx.fee,
          assets = Map(assetId -> -tx.amount))),
        assetInfos = Map(assetId -> AssetInfo(isReissuable = true, volume = -tx.amount)))
    })
  }
}
