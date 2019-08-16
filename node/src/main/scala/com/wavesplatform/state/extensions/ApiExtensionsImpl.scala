package com.wavesplatform.state.extensions

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.database.extensions.impl.{LevelDBWriterAddressTransactions, LevelDBWriterDistributions}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.impl.{CompositeAddressTransactions, CompositeDistributions}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, Blockchain, BlockchainUpdaterImpl, Height, Portfolio}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Asset, Transaction, TransactionParser}
import monix.reactive.Observable

object ApiExtensionsImpl {
  def apply(b: Blockchain): ApiExtensions = b match {
    case ldb: LevelDBWriter =>
      fromAddressTransactionsAndDistributions(LevelDBWriterAddressTransactions(ldb), LevelDBWriterDistributions(ldb))

    case upd: BlockchainUpdaterImpl =>
      val baseAddressTransactions = LevelDBWriterAddressTransactions(com.wavesplatform.state.extractLevelDB(upd))
      val baseDistributions       = LevelDBWriterDistributions(com.wavesplatform.state.extractLevelDB(upd))
      val addressTransactions     = new CompositeAddressTransactions(baseAddressTransactions, () => upd.bestLiquidDiff)
      val distributions           = new CompositeDistributions(upd, baseDistributions, () => upd.bestLiquidDiff)
      fromAddressTransactionsAndDistributions(addressTransactions, distributions)

    case cb: CompositeBlockchain =>
      val baseAddressTransactions = LevelDBWriterAddressTransactions(com.wavesplatform.state.extractLevelDB(cb))
      val baseDistributions       = LevelDBWriterDistributions(com.wavesplatform.state.extractLevelDB(cb))
      val addressTransactions     = new CompositeAddressTransactions(baseAddressTransactions, () => cb.maybeDiff)
      val distributions           = new CompositeDistributions(cb, baseDistributions, () => cb.maybeDiff)
      fromAddressTransactionsAndDistributions(addressTransactions, distributions)

    case _ =>
      fromAddressTransactionsAndDistributions(AddressTransactions.empty, Distributions.empty)
  }

  private[this] def fromAddressTransactionsAndDistributions(at: AddressTransactions, d: Distributions): ApiExtensions = {
    new AddressTransactions with Distributions {
      override def portfolio(a: Address): Portfolio                               = d.portfolio(a)
      override def assetDistribution(asset: Asset.IssuedAsset): AssetDistribution = d.assetDistribution(asset)
      override def assetDistributionAtHeight(asset: Asset.IssuedAsset,
                                             height: Int,
                                             count: Int,
                                             fromAddress: Option[Address]): Either[ValidationError, AssetDistributionPage] =
        d.assetDistributionAtHeight(asset, height, count, fromAddress)
      override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]]                    = d.wavesDistribution(height)
      override def nftObservable(address: Address, from: Option[Asset.IssuedAsset]): Observable[IssueTransaction] = d.nftObservable(address, from)
      override def addressTransactionsObservable(address: Address,
                                                 types: Set[TransactionParser],
                                                 fromId: Option[ByteStr]): Observable[(Height, Transaction)] =
        at.addressTransactionsObservable(address, types, fromId)
    }
  }
}
