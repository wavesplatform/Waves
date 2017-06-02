package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{AssetInfo, Diff, EqByteArray, LeaseInfo, Portfolio}
import scorex.account.AddressScheme
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, MakeAssetNameUniqueTransaction, ReissueTransaction}
import scorex.transaction.{AssetId, SignedTransaction, StateValidationError, Transaction}

import scala.util.{Left, Right}

object AssetTransactionsDiff {

  def issue(state: StateReader, height: Int)(tx: IssueTransaction): Either[StateValidationError, Diff] = {
    val info = AssetInfo(
      isReissuable = tx.reissuable,
      volume = tx.quantity)
    Right(Diff(height = height,
      tx = tx,
      portfolios = Map(tx.sender.toAccount -> Portfolio(
        balance = -tx.fee,
        leaseInfo = LeaseInfo.empty,
        assets = Map(tx.assetId -> tx.quantity))),
      assetInfos = Map(tx.assetId -> info)))
  }

  def reissue(state: StateReader, settings: FunctionalitySettings, blockTime: Long, height: Int)(tx: ReissueTransaction): Either[StateValidationError, Diff] = {
    findReferencedAsset(tx, state, tx.assetId).flatMap(itx => {
      val oldInfo = state.assetInfo(tx.assetId).get
      if (oldInfo.isReissuable || blockTime <= settings.allowInvalidReissueInSameBlockUntilTimestamp) {
        Right(Diff(height = height,
          tx = tx,
          portfolios = Map(tx.sender.toAccount -> Portfolio(
            balance = -tx.fee,
            leaseInfo = LeaseInfo.empty,
            assets = Map(tx.assetId -> tx.quantity))),
          assetInfos = Map(tx.assetId -> AssetInfo(
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
      Diff(height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAccount -> Portfolio(
          balance = -tx.fee,
          leaseInfo = LeaseInfo.empty,
          assets = Map(tx.assetId -> -tx.amount))),
        assetInfos = Map(tx.assetId -> AssetInfo(isReissuable = true, volume = -tx.amount)))
    })
  }

  def makeAssetNameUnique(state: StateReader, height: Int)(tx: MakeAssetNameUniqueTransaction): Either[StateValidationError, Diff] = {
    checkNetworkByte(tx.networkByte, tx).flatMap(tx =>
      findReferencedAsset(tx, state, tx.assetId).flatMap(itx => {
        val assetName = EqByteArray(itx.name)
        state.getAssetIdByUniqueName(assetName) match {
          case Some(assetId) =>
            Left(TransactionValidationError(tx, s"Asset name has been verified for ${Base58.encode(assetId.arr)}"))
          case None =>
            Right(Diff(height = height,
              tx = tx,
              portfolios = Map(tx.sender.toAccount -> Portfolio(
                balance = -tx.fee,
                leaseInfo = LeaseInfo.empty,
                assets = Map.empty)),
              assetsWithUniqueNames = Map(assetName -> tx.assetId)
            ))
        }
      }))
  }

  private def findReferencedAsset(tx: SignedTransaction, state: StateReader, assetId: AssetId): Either[StateValidationError, IssueTransaction] = {
    state.findTransaction[IssueTransaction](assetId) match {
      case None => Left(TransactionValidationError(tx, "Referenced assetId not found"))
      case Some(itx) if !(itx.sender equals tx.sender) => Left(TransactionValidationError(tx, "Asset was issued by other address"))
      case Some(itx) => Right(itx)
    }
  }

  private def checkNetworkByte[T <: Transaction](chainIdFromTx: Byte, tx: T): Either[StateValidationError, T] = {
    if (chainIdFromTx != AddressScheme.current.chainId) {
      Left(TransactionValidationError(tx, s"Invalid network byte '$chainIdFromTx', current '${AddressScheme.current.chainId}'"))
    } else Right(tx)
  }
}
