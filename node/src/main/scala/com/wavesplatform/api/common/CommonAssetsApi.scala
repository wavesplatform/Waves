package com.wavesplatform.api.common

import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction

class CommonAssetsApi(blockchain: Blockchain) {
  import CommonAssetsApi._

  def description(assetId: IssuedAsset): Option[AssetDescription] = {
    blockchain.assetDescription(assetId)
  }

  def fullInfo(assetId: IssuedAsset): Option[AssetInfo] =
    for {
      assetInfo <- blockchain.assetDescription(assetId)
      issueTransaction = blockchain.transactionInfo(assetId.id).collect { case (_, tx: IssueTransaction) => tx }
      sponsorBalance = issueTransaction match {
        case Some(tx) if assetInfo.sponsorship != 0 => Some(blockchain.wavesPortfolio(tx.sender).spendableBalance)
        case _                                      => None
      }
    } yield AssetInfo(assetInfo, issueTransaction, sponsorBalance)
}

object CommonAssetsApi {
  final case class AssetInfo(description: AssetDescription, issueTransaction: Option[IssueTransaction], sponsorBalance: Option[Long])
}
