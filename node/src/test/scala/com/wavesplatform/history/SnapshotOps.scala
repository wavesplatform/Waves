package com.wavesplatform.history

import cats.data.Ior
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.state.*
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.collection.immutable.VectorMap

object SnapshotOps {
  implicit class TransactionStateSnapshotExt(val s: StateSnapshot) extends AnyVal {

    def toDiff(blockchain: Blockchain): Diff =
      Diff.withTransactions(
        s.transactions.values.toVector,
        portfolios(blockchain),
        issuedAssets,
        updatedAssets,
        s.aliases,
        orderFills(blockchain),
        s.leaseStates,
        s.accountScripts,
        s.assetScripts,
        s.accountData,
        s.sponsorships,
        scriptsRun = 0,
        s.scriptsComplexity,
        s.scriptResults,
        s.ethereumTransactionMeta
      )

    private def portfolios(blockchain: Blockchain): Map[Address, Portfolio] =
      Diff.combine(balancePortfolios(blockchain), leasePortfolios(blockchain)).explicitGet()

    private def balancePortfolios(blockchain: Blockchain): Map[Address, Portfolio] =
      s.balances
        .foldLeft(Map[Address, Portfolio]()) { case (portfolios, ((address, asset), balance)) =>
          val portfolio = Portfolio.build(asset, balance - blockchain.balance(address, asset))
          Diff.combine(portfolios, Map(address -> portfolio)).explicitGet()
        }

    private def leasePortfolios(blockchain: Blockchain): Map[Address, Portfolio] =
      s.leaseBalances
        .map { case (address, current) =>
          val init = blockchain.leaseBalance(address)
          address -> Portfolio(lease = LeaseBalance(in = current.in - init.in, out = current.out - init.out))
        }

    private def issuedAssets: VectorMap[IssuedAsset, NewAssetInfo] =
      VectorMap[IssuedAsset, NewAssetInfo]() ++ s.assetStatics.map { case (asset, pbStatic) =>
        val static = AssetStaticInfo(
          asset.id,
          pbStatic.sourceTransactionId.toTxId,
          pbStatic.issuerPublicKey.toPublicKey,
          pbStatic.decimals,
          pbStatic.nft
        )
        asset -> NewAssetInfo(static, s.assetNamesAndDescriptions(asset), s.assetVolumes(asset))
      }

    private def updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]] =
      (s.assetVolumes.keySet ++ s.assetNamesAndDescriptions.keySet)
        .filterNot(issuedAssets.contains)
        .map { asset =>
          val info =
            (s.assetNamesAndDescriptions.get(asset), s.assetVolumes.get(asset)) match {
              case (Some(dynamic), Some(volume)) => Ior.Both(dynamic, volume)
              case (Some(dynamic), None)         => Ior.Left(dynamic)
              case (None, Some(volume))          => Ior.Right(volume)
              case _                             => ???
            }
          asset -> info
        }
        .toMap

    private def orderFills(blockchain: Blockchain): Map[ByteStr, VolumeAndFee] =
      s.orderFills.map { case (orderId, info) =>
        val init = blockchain.filledVolumeAndFee(orderId)
        orderId -> VolumeAndFee(info.volume - init.volume, info.fee - init.fee)
      }
  }
}
