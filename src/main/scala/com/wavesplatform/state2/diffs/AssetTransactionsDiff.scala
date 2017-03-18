package com.wavesplatform.state2.diffs

import com.wavesplatform.state2.{AssetInfo, ByteArray, Diff, EqByteArray, Portfolio, StateReader}
import scorex.account.Account
import scorex.transaction.{SignedTransaction, StateValidationError, Transaction}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}

import scala.util.{Left, Right}

object AssetTransactionsDiff {

  def issue(state: StateReader, height: Int)(tx: IssueTransaction): Either[StateValidationError, Diff] = {
    val assetId = EqByteArray(tx.assetId)
    val info = AssetInfo(
      isReissuable = tx.reissuable,
      totalVolume = tx.quantity)
    Right(Diff(height = height,
      tx = tx,
      portfolios = Map(Account.fromPublicKey(tx.sender.publicKey) -> Portfolio(
        balance = -tx.fee,
        effectiveBalance = -tx.fee,
        assets = Map(assetId -> tx.quantity))),
      assetInfos = Map(assetId -> info)))
  }

  def reissue(state: StateReader, height: Int)(tx: ReissueTransaction): Either[StateValidationError, Diff] = {
    val issueTxEi = state.findTransaction[IssueTransaction](tx.assetId) match {
      case None => Left(TransactionValidationError(tx, "Referenced assetId not found"))
      case Some(itx) if !(itx.sender equals tx.sender) => Left(TransactionValidationError(tx, "Asset was issued by other address"))
      case Some(itx) => Right(itx)
    }
    issueTxEi.flatMap(itx => {
      val assetId = EqByteArray(tx.assetId)
      val oldInfo = state.assetInfo(assetId).get
      if (oldInfo.isReissuable) {
        val newInfo = AssetInfo(
          totalVolume = oldInfo.totalVolume + tx.quantity,
          isReissuable = itx.reissuable)
        Right(Diff(height = height,
          tx = tx,
          portfolios = Map(Account.fromPublicKey(tx.sender.publicKey) -> Portfolio(
            balance = -tx.fee,
            effectiveBalance = -tx.fee,
            assets = Map(assetId -> tx.quantity))),
          assetInfos = Map(assetId -> newInfo)))
      } else {
        Left(TransactionValidationError(tx, "Asset is not reissuable"))
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
      val oldInfo = state.assetInfo(assetId).get
      val newInfo = oldInfo.copy(totalVolume = oldInfo.totalVolume - tx.amount)
      Diff(height = height,
        tx = tx,
        portfolios = Map(Account.fromPublicKey(tx.sender.publicKey) -> Portfolio(
          balance = -tx.fee,
          effectiveBalance = -tx.fee,
          assets = Map(assetId -> -tx.amount))),
        assetInfos = Map(assetId -> newInfo))
    })
  }
}
