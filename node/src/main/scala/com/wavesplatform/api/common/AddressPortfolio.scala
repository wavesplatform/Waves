package com.wavesplatform.api.common

import cats.syntax.semigroup.*
import com.google.common.collect.AbstractIterator
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.database.{AddressId, DBResource, KeyTags, Keys, readIntSeq}
import com.wavesplatform.state.{AssetDescription, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.ScorexLogging

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

class NFTIterator(addressId: AddressId, maybeAfter: Option[IssuedAsset], resource: DBResource)
    extends AbstractIterator[(IssuedAsset, Long)]
    with ScorexLogging {
  private val prefixBytes = KeyTags.NftPossession.prefixBytes ++ addressId.toByteArray

  resource.iterator.seek(prefixBytes)

  for (after <- maybeAfter) {
    @inline
    def skipEntry(key: Array[Byte]): Boolean =
      key.startsWith(prefixBytes) && !key.endsWith(after.id.arr)

    while (resource.iterator.isValid && skipEntry(resource.iterator.key())) {}
  }

  override def computeNext(): (IssuedAsset, Long) =
    if (resource.iterator.isValid && resource.iterator.key().startsWith(prefixBytes)) {
      val assetId = IssuedAsset(ByteStr(resource.iterator.key().takeRight(crypto.DigestLength)))
      resource.iterator.next()
      assetId -> (for {
        lastChange <- resource.get(Keys.assetBalanceHistory(addressId, assetId)).headOption
      } yield resource.get(Keys.assetBalance(addressId, assetId)(lastChange))).getOrElse(0L)
    } else endOfData()
}

class AssetBalanceIterator(addressId: AddressId, resource: DBResource) extends AbstractIterator[(IssuedAsset, Long)] {
  private val prefixBytes = KeyTags.AssetBalanceHistory.prefixBytes ++ addressId.toByteArray

  resource.iterator.seek(prefixBytes)

  private def stillSameAddress(k: Array[Byte]): Boolean =
    (k.length == (prefixBytes.length + crypto.DigestLength)) && k.startsWith(prefixBytes)

  override def computeNext(): (IssuedAsset, Long) =
    if (resource.iterator.isValid && stillSameAddress(resource.iterator.key())) {
      val assetId      = IssuedAsset(ByteStr(resource.iterator.key().takeRight(crypto.DigestLength)))
      val history      = readIntSeq(resource.iterator.value())
      val balance      = resource.get(Keys.assetBalance(addressId, assetId)(history.headOption.getOrElse(0)))
      resource.iterator.next()
      assetId -> balance
    } else endOfData()
}

class BalanceIterator(
    underlying: Iterator[(IssuedAsset, Long)],
    includeAsset: IssuedAsset => Boolean,
    private var pendingOverrides: Map[IssuedAsset, Long]
) extends AbstractIterator[(IssuedAsset, Long)] {

  @tailrec private def nextOverride(): (IssuedAsset, Long) =
    if (pendingOverrides.isEmpty) endOfData()
    else {
      val head @ (asset, _) = pendingOverrides.head
      pendingOverrides -= asset
      if (includeAsset(asset)) head else nextOverride()
    }

  override def computeNext(): (IssuedAsset, Long) =
    if (underlying.hasNext) {
      val (asset, balanceFromHistory) = underlying.next()
      val balanceFromDiff             = pendingOverrides.getOrElse(asset, 0L)
      pendingOverrides -= asset
      asset -> (balanceFromDiff |+| balanceFromHistory)
    } else nextOverride()
}

object AddressPortfolio {
  def nftIterator(
      resource: DBResource,
      address: Address,
      diff: Diff,
      maybeAfter: Option[IssuedAsset],
      loadAssetDescription: IssuedAsset => Option[AssetDescription]
  ): Iterator[(IssuedAsset, AssetDescription)] =
    new BalanceIterator(
      resource
        .get(Keys.addressId(address))
        .fold[Iterator[(IssuedAsset, Long)]](Iterator())(addressId => new NFTIterator(addressId, maybeAfter, resource).asScala),
      asset => loadAssetDescription(asset).exists(_.nft),
      diff.portfolios.getOrElse(address, Portfolio.empty).assets
    ).asScala
      .collect { case (asset, balance) if balance > 0 => asset }
      .flatMap(a => loadAssetDescription(a).map(a -> _))

  def assetBalanceIterator(
      resource: DBResource,
      address: Address,
      diff: Diff,
      includeAsset: IssuedAsset => Boolean
  ): Iterator[(IssuedAsset, Long)] =
    new BalanceIterator(
      resource
        .get(Keys.addressId(address))
        .fold[Iterator[(IssuedAsset, Long)]](Iterator())(addressId => new AssetBalanceIterator(addressId, resource).asScala),
      includeAsset,
      diff.portfolios.getOrElse(address, Portfolio.empty).assets
    ).asScala.filter {
      case (asset, balance) => includeAsset(asset) && balance > 0
    }
}
