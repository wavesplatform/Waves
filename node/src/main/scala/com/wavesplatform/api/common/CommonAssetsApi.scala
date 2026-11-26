package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonAssetsApi.AssetInfo
import com.wavesplatform.database.{KeyTag, Keys, RDB}
import com.wavesplatform.state.{AssetDescription, Blockchain, StateSnapshot, TxMeta}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import monix.reactive.Observable

trait CommonAssetsApi {
  def description(assetId: IssuedAsset): Option[AssetDescription]

  def fullInfo(assetId: IssuedAsset): Option[AssetInfo]

  def fullInfos(assetIds: Seq[IssuedAsset]): Seq[Option[AssetInfo]]

  def wavesDistribution(height: Int, after: Option[Address]): Observable[(Address, Long)]

  def assetDistribution(asset: IssuedAsset, height: Int, after: Option[Address]): Observable[(Address, Long)]
}

object CommonAssetsApi {
  final case class AssetInfo(description: AssetDescription, issueTransaction: Option[IssueTransaction], sponsorBalance: Option[Long])

  def apply(snapshot: () => StateSnapshot, rdb: RDB, blockchain: Blockchain): CommonAssetsApi = new CommonAssetsApi {
    def description(assetId: IssuedAsset): Option[AssetDescription] =
      blockchain.assetDescription(assetId)

    def fullInfo(assetId: IssuedAsset): Option[AssetInfo] =
      for {
        assetInfo <- blockchain.assetDescription(assetId)
        sponsorBalance = if (assetInfo.sponsorship != 0) Some(blockchain.wavesPortfolio(assetInfo.issuer.toAddress).spendableBalance) else None
      } yield AssetInfo(
        assetInfo,
        blockchain.transactionInfo(assetId.id).collect { case (tm, it: IssueTransaction) if tm.status == TxMeta.Status.Succeeded => it },
        sponsorBalance
      )

    override def fullInfos(assetIds: Seq[IssuedAsset]): Seq[Option[AssetInfo]] = {
      blockchain
        .transactionInfos(assetIds.map(_.id))
        .view
        .zip(assetIds)
        .map { case (tx, assetId) =>
          blockchain.assetDescription(assetId).map { desc =>
            AssetInfo(
              desc,
              tx.collect { case (tm, it: IssueTransaction) if tm.status == TxMeta.Status.Succeeded => it },
              if (desc.sponsorship != 0) Some(blockchain.wavesPortfolio(desc.issuer.toAddress).spendableBalance) else None
            )
          }
        }
        .toSeq
    }

    override def wavesDistribution(height: Int, after: Option[Address]): Observable[(Address, Long)] =
      balanceDistribution(
        rdb,
        height,
        after,
        if (height == blockchain.height) snapshot().balances else Map(),
        KeyTag.WavesBalance.prefixBytes,
        Waves
      )

    override def assetDistribution(asset: IssuedAsset, height: Int, after: Option[Address]): Observable[(Address, Long)] =
      balanceDistribution(
        rdb,
        height,
        after,
        if (height == blockchain.height) snapshot().balances else Map(),
        Keys.assetBalanceHolderPrefix(asset),
        asset
      )
  }
}
