package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonAssetsApi.AssetInfo
import com.wavesplatform.crypto
import com.wavesplatform.database.{AddressId, KeyTags}
import com.wavesplatform.state.{AssetDescription, Blockchain, Diff, Portfolio, TxMeta}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import monix.reactive.Observable
import org.rocksdb.RocksDB

trait CommonAssetsApi {
  def description(assetId: IssuedAsset): Option[AssetDescription]

  def fullInfo(assetId: IssuedAsset): Option[AssetInfo]

  def fullInfos(assetIds: Seq[IssuedAsset]): Seq[Option[AssetInfo]]

  def wavesDistribution(height: Int, after: Option[Address]): Observable[(Address, Long)]

  def assetDistribution(asset: IssuedAsset, height: Int, after: Option[Address]): Observable[(Address, Long)]
}

object CommonAssetsApi {
  final case class AssetInfo(description: AssetDescription, issueTransaction: Option[IssueTransaction], sponsorBalance: Option[Long])

  def apply(diff: () => Diff, db: RocksDB, blockchain: Blockchain): CommonAssetsApi = new CommonAssetsApi {
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
        db,
        height,
        after,
        if (height == blockchain.height) diff().portfolios else Map.empty[Address, Portfolio],
        KeyTags.WavesBalanceHistory.prefixBytes,
        bs => AddressId.fromByteArray(bs.slice(2, bs.length - 4)),
        _.balance
      )

    override def assetDistribution(asset: IssuedAsset, height: Int, after: Option[Address]): Observable[(Address, Long)] =
      balanceDistribution(
        db,
        height,
        after,
        if (height == blockchain.height) diff().portfolios else Map.empty[Address, Portfolio],
        KeyTags.AssetBalanceHistory.prefixBytes ++ asset.id.arr,
        bs => AddressId.fromByteArray(bs.slice(2 + crypto.DigestLength, bs.length - 4)),
        _.assets.getOrElse(asset, 0L)
      )
  }
}
