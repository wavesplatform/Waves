package com.wavesplatform.api.common

import com.google.common.collect.AbstractIterator
import com.google.common.primitives.Ints
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.NFTIterator.BatchSize
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.database.{AddressId, DBResource, KeyTags, Keys}
import com.wavesplatform.state.{AssetDescription, StateSnapshot}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.ScorexLogging

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

class NFTIterator(addressId: AddressId, maybeAfter: Option[IssuedAsset], resource: DBResource)
    extends AbstractIterator[Seq[IssuedAsset]]
    with ScorexLogging {
  private val prefixBytes: Array[Byte] = KeyTags.NftPossession.prefixBytes ++ addressId.toByteArray

  resource.withSafePrefixIterator { dbIterator =>
    dbIterator.seek(prefixBytes)

    for (after <- maybeAfter) {
      @inline
      def skipEntry(key: Array[Byte]): Boolean =
        !key.endsWith(after.id.arr)

      while (dbIterator.isValid && skipEntry(dbIterator.key())) {
        dbIterator.next()
      }
      if (dbIterator.isValid && !skipEntry(dbIterator.key()))
        dbIterator.next()
    }
  }(())

  override def computeNext(): Seq[IssuedAsset] =
    resource.withSafePrefixIterator { dbIterator =>
      val assetsBuffer = new ArrayBuffer[IssuedAsset]()
      while (dbIterator.isValid && assetsBuffer.length < BatchSize) {
        val assetId = IssuedAsset(ByteStr(dbIterator.key().takeRight(crypto.DigestLength)))
        assetsBuffer.addOne(assetId)
        dbIterator.next()
      }
      if (assetsBuffer.nonEmpty) {
        assetsBuffer.toSeq
      } else endOfData()
    }(endOfData())
}

object NFTIterator {
  val BatchSize = 1000
}

class AssetBalanceIterator(addressId: AddressId, resource: DBResource) extends AbstractIterator[Seq[IssuedAsset]] {
  private val prefixBytes: Array[Byte] = KeyTags.AssetBalance.prefixBytes ++ addressId.toByteArray

  resource.withSafePrefixIterator(_.seek(prefixBytes))(())

  override def computeNext(): Seq[IssuedAsset] =
    resource.withSafePrefixIterator { dbIterator =>
      if (dbIterator.isValid) {
        val assetId    = IssuedAsset(ByteStr(dbIterator.key().takeRight(crypto.DigestLength)))
        dbIterator.next()
        Seq(assetId)
      } else endOfData()
    }(endOfData())
}

class WavesBalanceIterator(addressId: AddressId, resource: DBResource) extends AbstractIterator[(Int, Long)] {
  private val prefixBytes: Array[Byte] = KeyTags.WavesBalanceHistory.prefixBytes ++ addressId.toByteArray
  private val lastHeight: Int          = resource.get(Keys.wavesBalance(addressId)).height.toInt

  resource.withSafePrefixIterator(_.seekForPrev(prefixBytes ++ Ints.toByteArray(lastHeight)))(())

  override def computeNext(): (Int, Long) =
    resource.withSafePrefixIterator { dbIterator =>
      if (dbIterator.isValid) {
        val h       = ByteBuffer.wrap(dbIterator.key().drop(prefixBytes.length)).getInt
        val balance = ByteBuffer.wrap(dbIterator.value()).getLong
        dbIterator.prev()
        h -> balance
      } else endOfData()
    }(endOfData())
}

class BalanceIterator(
    address: Address,
    underlying: Iterator[Seq[IssuedAsset]],
    include: IssuedAsset => Boolean,
    private var pendingOverrides: Map[(Address, Asset), Long]
) extends AbstractIterator[Seq[(IssuedAsset, Long)]] {

  private def nextOverride(): Seq[(IssuedAsset, Long)] =
    if (pendingOverrides.isEmpty) endOfData()
    else {
      val balances = pendingOverrides.collect {
        case ((`address`, asset: IssuedAsset), balance) if include(asset) =>
          asset -> balance
      }.toSeq
      pendingOverrides = Map.empty
      balances
    }

  override def computeNext(): Seq[(IssuedAsset, Long)] =
    if (underlying.hasNext) {
      underlying.next().map { asset =>
        val key     = (address, asset)
        val balance = pendingOverrides.getOrElse(key, 0L)
        pendingOverrides -= key
        asset -> balance
      }
    } else nextOverride()
}

object AddressPortfolio {
  def nftIterator(
      resource: DBResource,
      address: Address,
      snapshot: StateSnapshot,
      maybeAfter: Option[IssuedAsset],
      loadAssetDescription: IssuedAsset => Option[AssetDescription]
  ): Iterator[Seq[(IssuedAsset, AssetDescription)]] =
    new BalanceIterator(
      address,
      resource
        .get(Keys.addressId(address))
        .fold[Iterator[Seq[IssuedAsset]]](Iterator.empty)(addressId => new NFTIterator(addressId, maybeAfter, resource).asScala),
      asset => loadAssetDescription(asset).exists(_.nft),
      snapshot.balances
    ).asScala
      .map(_.collect { case (asset, balance) if balance > 0 => asset }
        .flatMap(a => loadAssetDescription(a).map(a -> _)))

  def assetBalanceIterator(
      resource: DBResource,
      address: Address,
      snapshot: StateSnapshot,
      includeAsset: IssuedAsset => Boolean
  ): Iterator[Seq[(IssuedAsset, Long)]] =
    new BalanceIterator(
      address,
      resource
        .get(Keys.addressId(address))
        .fold[Iterator[Seq[IssuedAsset]]](Iterator.empty)(addressId => new AssetBalanceIterator(addressId, resource).asScala),
      includeAsset,
      snapshot.balances
    ).asScala
      .map(_.filter { case (asset, balance) =>
        includeAsset(asset) && balance > 0
      })
}
