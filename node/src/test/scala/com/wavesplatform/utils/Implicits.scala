package com.wavesplatform.utils

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.database.extensions.impl.LevelDBApiExtensions
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.ApiExtensions
import com.wavesplatform.state.extensions.impl.CompositeApiExtensions
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, Blockchain, Height, Portfolio}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Asset, Transaction, TransactionParser}
import monix.reactive.Observable

object Implicits {
  implicit def blockchainToApiExtensions(b: Blockchain): ApiExtensions = b match {
    case ldb: LevelDBWriter => new LevelDBApiExtensions(ldb)
    case cb: CompositeBlockchain =>
      new CompositeApiExtensions(cb, new LevelDBApiExtensions(cb.stableBlockchain.asInstanceOf[LevelDBWriter]), () => cb.maybeDiff)
    case _ =>
      new ApiExtensions {
        override def portfolio(a: Address): Portfolio               = Portfolio.empty
        override def accountDataKeys(address: Address): Set[String] = Set.empty
        override def addressTransactionsObservable(
            address: Address,
            types: Set[TransactionParser],
            fromId: Option[ByteStr]
        ): Observable[(Height, Transaction)]                                        = Observable.empty
        override def assetDistribution(asset: Asset.IssuedAsset): AssetDistribution = AssetDistribution(Map.empty[Address, Long])
        override def assetDistributionAtHeight(
            asset: Asset.IssuedAsset,
            height: Int,
            count: Int,
            fromAddress: Option[Address]
        ): Either[ValidationError, AssetDistributionPage]                                                           = Right(AssetDistributionPage(Paged(hasNext = false, Option.empty[Address], AssetDistribution(Map.empty[Address, Long]))))
        override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]]                    = Right(Map.empty)
        override def nftObservable(address: Address, from: Option[Asset.IssuedAsset]): Observable[IssueTransaction] = Observable.empty
      }
  }
}
