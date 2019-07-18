package com.wavesplatform.state.extensions

import com.wavesplatform.account.Address
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{AssetDistribution, AssetDistributionPage, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.utils.CloseableIterator
import monix.reactive.Observable

trait Distributions {
  def portfolio(a: Address): Portfolio

  def assetDistribution(asset: IssuedAsset): AssetDistribution
  def assetDistributionAtHeight(asset: IssuedAsset,
                                height: Int,
                                count: Int,
                                fromAddress: Option[Address]): Either[ValidationError, AssetDistributionPage]

  def wavesDistribution(height: Int): Either[ValidationError, Map[Address, Long]]

  def nftIterator(address: Address, from: Option[IssuedAsset]): CloseableIterator[IssueTransaction]

  final def nftObs(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction] =
    Observable.defer {
      val iterator = nftIterator(address, from)
      Observable.fromIterator(iterator, () => iterator.close())
    }

  final def nftList(address: Address, from: Option[IssuedAsset], count: Int): Seq[IssueTransaction] =
    nftIterator(address, from)
      .take(count)
      .closeAfter(_.toVector)
}
