package com.wavesplatform.state2.diffs

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{AssetInfo, ByteStr, Diff, LeaseInfo, Portfolio}
import scorex.account.AddressScheme
import scorex.transaction.ValidationError.{GenericError}
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, MakeAssetNameUniqueTransaction, ReissueTransaction}
import scorex.transaction.{AssetId, SignedTransaction, Transaction, ValidationError}

import scala.util.{Left, Right}

object AssetTransactionsDiff {

  def issue(state: StateReader, height: Int)(tx: IssueTransaction): Either[ValidationError, Diff] = {
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

  def reissue(state: StateReader, settings: FunctionalitySettings, blockTime: Long, height: Int)(tx: ReissueTransaction): Either[ValidationError, Diff] = {
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
        Left(GenericError(s"Asset is not reissuable and blockTime=$blockTime is greater than " +
          s"settings.allowInvalidReissueInSameBlockUntilTimestamp=${settings.allowInvalidReissueInSameBlockUntilTimestamp}"))
      }
    })
  }

  def burn(state: StateReader, height: Int)(tx: BurnTransaction): Either[ValidationError, Diff] = {
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

  def makeAssetNameUnique(state: StateReader, height: Int)(tx: MakeAssetNameUniqueTransaction): Either[ValidationError, Diff] = {
    checkNetworkByte(tx.networkByte, tx).flatMap(tx =>
      findReferencedAsset(tx, state, tx.assetId).flatMap(itx => {
        val assetName = ByteStr(itx.name)
        state.getAssetIdByUniqueName(assetName) match {
          case Some(assetId) =>
            Left(GenericError(s"Asset name has been verified for ${assetId.base58}"))
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

  private def findReferencedAsset(tx: SignedTransaction, state: StateReader, assetId: AssetId): Either[ValidationError, IssueTransaction] = {
    state.findTransaction[IssueTransaction](assetId) match {
      case None => Left(GenericError("Referenced assetId not found"))
      case Some(itx) if !(itx.sender equals tx.sender) => Left(GenericError("Asset was issued by other address"))
      case Some(itx) => Right(itx)
    }
  }

  private def checkNetworkByte[T <: Transaction](chainIdFromTx: Byte, tx: T): Either[ValidationError, T] = {
    if (chainIdFromTx != AddressScheme.current.chainId) {
      Left(GenericError(s"Invalid network byte '$chainIdFromTx', current '${AddressScheme.current.chainId}'"))
    } else Right(tx)
  }
}
