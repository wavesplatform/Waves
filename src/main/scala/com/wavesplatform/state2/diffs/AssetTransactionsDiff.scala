package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{AssetInfo, Diff, EqByteArray, LeaseInfo, Portfolio}
import scorex.account.Account
import scorex.crypto.encode.Base58
import scorex.transaction.{SignedTransaction, StateValidationError, Transaction}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, MakeAssetNameUniqueTransaction, ReissueTransaction}

import scala.util.{Left, Right}

object AssetTransactionsDiff {

  def issue(state: StateReader, height: Int)(tx: IssueTransaction): Either[StateValidationError, Diff] = {
    val assetId = EqByteArray(tx.assetId)
    val info = AssetInfo(
      isReissuable = tx.reissuable,
      volume = tx.quantity)
    Right(Diff(height = height,
      tx = tx,
      portfolios = Map(tx.sender.toAccount -> Portfolio(
        balance = -tx.fee,
        leaseInfo = LeaseInfo.empty,
        assets = Map(assetId -> tx.quantity))),
      assetInfos = Map(assetId -> info)))
  }

  def reissue(state: StateReader, settings: FunctionalitySettings, blockTime: Long, height: Int)(tx: ReissueTransaction): Either[StateValidationError, Diff] = {
    findReferencedAsset(tx, state, tx.assetId).flatMap(itx => {
      val assetId = EqByteArray(tx.assetId)
      val oldInfo = state.assetInfo(assetId).get
      if (oldInfo.isReissuable || blockTime <= settings.allowInvalidReissueInSameBlockUntilTimestamp) {
        Right(Diff(height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAccount -> Portfolio(
            balance = -tx.fee,
            leaseInfo = LeaseInfo.empty,
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
    findReferencedAsset(tx, state, tx.assetId).map(itx => {
      val assetId = EqByteArray(tx.assetId)
      Diff(height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAccount -> Portfolio(
          balance = -tx.fee,
          leaseInfo = LeaseInfo.empty,
          assets = Map(assetId -> -tx.amount))),
              assetInfos = Map(assetId -> AssetInfo(isReissuable = true, volume = -tx.amount)))
    })
  }

  def makeUnique(state: StateReader, height: Int)(tx: MakeAssetNameUniqueTransaction): Either[StateValidationError, Diff] = {
    findReferencedAsset(tx, state, tx.assetId).flatMap(itx => {
      val assetName = EqByteArray(itx.name)
      state.getAssetIdByUniqueName(assetName) match {
        case Some(assetId) =>
          Left(TransactionValidationError(tx, s"Asset name has been verified for ${Base58.encode(assetId.arr)}"))
        case None =>
          val assetId = EqByteArray(tx.assetId)
          Right(Diff(height = height,
            tx = tx,
            portfolios = Map(tx.sender.toAccount -> Portfolio(
              balance = -tx.fee,
              leaseInfo = LeaseInfo.empty,
              assets = Map.empty)),
            assetsWithUniqueNames = Map(assetName -> assetId)
          ))
      }
    })
  }

  private def findReferencedAsset(tx: SignedTransaction, state: StateReader, assetId: Array[Byte]): Either[StateValidationError, IssueTransaction] = {
    state.findTransaction[IssueTransaction](assetId) match {
      case None => Left(TransactionValidationError(tx, "Referenced assetId not found"))
      case Some(itx) if !(itx.sender equals tx.sender) => Left(TransactionValidationError(tx, "Asset was issued by other address"))
      case Some(itx) => Right(itx)
    }
  }
}
