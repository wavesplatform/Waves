package com.wavesplatform.state.extensions

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.database.extensions.impl.{LevelDBAccountAggregations, LevelDBAddressTransactions, LevelDBDistributions}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.extensions.impl.{CompositeAccountAggregations, CompositeAddressTransactions, CompositeDistributions}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, Blockchain, Height, Portfolio}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Asset, Transaction, TransactionParser}
import monix.reactive.Observable

object ApiExtensionsImpl {
  def fromLevelDB(ldb: LevelDBWriter): ApiExtensions =
    compose(LevelDBAddressTransactions(ldb), LevelDBDistributions(ldb), LevelDBAccountAggregations(ldb))

  def fromCompositeBlockchain(cb: CompositeBlockchain): ApiExtensions = {
    val baseProvider        = fromLevelDB(cb.stableBlockchain.asInstanceOf[LevelDBWriter])
    val addressTransactions = new CompositeAddressTransactions(baseProvider, () => cb.maybeDiff)
    val distributions       = new CompositeDistributions(cb, baseProvider, () => cb.maybeDiff)
    val aggregations        = new CompositeAccountAggregations(cb, baseProvider, () => cb.maybeDiff)
    compose(addressTransactions, distributions, aggregations)
  }

  def apply(b: Blockchain): ApiExtensions = b match {
    case ldb: LevelDBWriter      => fromLevelDB(ldb)
    case cb: CompositeBlockchain => fromCompositeBlockchain(cb)
    case _                       => compose(AddressTransactions.empty, Distributions.empty, AccountAggregations.empty)
  }

  private[extensions] def compose(at: AddressTransactions, d: Distributions, aa: AccountAggregations): ApiExtensions = {
    new AddressTransactions with Distributions with AccountAggregations {
      override def portfolio(a: Address): Portfolio                               = aa.portfolio(a)
      override def assetDistribution(asset: Asset.IssuedAsset): AssetDistribution = d.assetDistribution(asset)
      override def assetDistributionAtHeight(
          asset: Asset.IssuedAsset,
          height: Int,
          count: Int,
          fromAddress: Option[Address]
      ): Either[ValidationError, AssetDistributionPage] =
        d.assetDistributionAtHeight(asset, height, count, fromAddress)
      override def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]]                    = d.wavesDistribution(height)
      override def nftObservable(address: Address, from: Option[Asset.IssuedAsset]): Observable[IssueTransaction] = d.nftObservable(address, from)
      override def addressTransactionsObservable(
          address: Address,
          types: Set[TransactionParser],
          fromId: Option[ByteStr]
      ): Observable[(Height, Transaction)] =
        at.addressTransactionsObservable(address, types, fromId)
      override def accountDataKeys(address: Address): Set[String] = aa.accountDataKeys(address)
    }
  }
}
