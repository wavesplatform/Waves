package com.wavesplatform.api.common

import cats.syntax.semigroup.*
import com.google.common.collect.AbstractIterator
import com.google.common.primitives.Ints
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.NFTIterator.BatchSize
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.database.{AddressId, CurrentBalance, DBResource, Key, KeyTags, Keys, readCurrentBalance}
import com.wavesplatform.state.{AssetDescription, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.ScorexLogging

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

class NFTIterator(addressId: AddressId, maybeAfter: Option[IssuedAsset], resource: DBResource)
    extends AbstractIterator[Seq[(IssuedAsset, Long)]]
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

  override def computeNext(): Seq[(IssuedAsset, Long)] =
    resource.withSafePrefixIterator { dbIterator =>
      val keysBuffer   = new ArrayBuffer[Key[CurrentBalance]]()
      val assetsBuffer = new ArrayBuffer[IssuedAsset]()
      while (dbIterator.isValid && keysBuffer.length < BatchSize) {
        val assetId = IssuedAsset(ByteStr(dbIterator.key().takeRight(crypto.DigestLength)))
        keysBuffer.addOne(Keys.assetBalance(addressId, assetId))
        assetsBuffer.addOne(assetId)
        dbIterator.next()
      }
      if (keysBuffer.nonEmpty) {
        resource
          .multiGet(keysBuffer, 16)
          .zip(assetsBuffer)
          .map { case (curBalance, asset) =>
            asset -> curBalance.balance
          }
          .toSeq
      } else endOfData()
    }(endOfData())
}

object NFTIterator {
  val BatchSize = 1000
}

class AssetBalanceIterator(addressId: AddressId, resource: DBResource) extends AbstractIterator[Seq[(IssuedAsset, Long)]] {
  private val prefixBytes: Array[Byte] = KeyTags.AssetBalance.prefixBytes ++ addressId.toByteArray

  resource.withSafePrefixIterator(_.seek(prefixBytes))(())

  override def computeNext(): Seq[(IssuedAsset, Long)] =
    resource.withSafePrefixIterator { dbIterator =>
      if (dbIterator.isValid) {
        val assetId    = IssuedAsset(ByteStr(dbIterator.key().takeRight(crypto.DigestLength)))
        val curBalance = readCurrentBalance(dbIterator.value())
        dbIterator.next()
        Seq(assetId -> curBalance.balance)
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
    underlying: Iterator[Seq[(IssuedAsset, Long)]],
    includeAsset: IssuedAsset => Boolean,
    private var pendingOverrides: Map[IssuedAsset, Long]
) extends AbstractIterator[Seq[(IssuedAsset, Long)]] {

  private def nextOverride(): Seq[(IssuedAsset, Long)] =
    if (pendingOverrides.isEmpty) endOfData()
    else {
      val balances = pendingOverrides.collect {
        case (asset, balance) if includeAsset(asset) =>
          asset -> balance
      }.toSeq
      pendingOverrides = Map.empty
      balances
    }

  override def computeNext(): Seq[(IssuedAsset, Long)] =
    if (underlying.hasNext) {
      underlying.next().map { case (asset, balanceFromHistory) =>
        val balanceFromDiff = pendingOverrides.getOrElse(asset, 0L)
        pendingOverrides -= asset
        asset -> (balanceFromDiff |+| balanceFromHistory)
      }
    } else nextOverride()
}

object AddressPortfolio {
  def nftIterator(
      resource: DBResource,
      address: Address,
      diff: Diff,
      maybeAfter: Option[IssuedAsset],
      loadAssetDescription: IssuedAsset => Option[AssetDescription]
  ): Iterator[Seq[(IssuedAsset, AssetDescription)]] =
    new BalanceIterator(
      resource
        .get(Keys.addressId(address))
        .fold[Iterator[Seq[(IssuedAsset, Long)]]](Iterator.empty)(addressId => new NFTIterator(addressId, maybeAfter, resource).asScala),
      asset => loadAssetDescription(asset).exists(_.nft),
      diff.portfolios.getOrElse(address, Portfolio.empty).assets
    ).asScala
      .map(_.collect { case (asset, balance) if balance > 0 => asset }
        .flatMap(a => loadAssetDescription(a).map(a -> _)))

  def assetBalanceIterator(
      resource: DBResource,
      address: Address,
      diff: Diff,
      includeAsset: IssuedAsset => Boolean
  ): Iterator[Seq[(IssuedAsset, Long)]] =
    new BalanceIterator(
      resource
        .get(Keys.addressId(address))
        .fold[Iterator[Seq[(IssuedAsset, Long)]]](Iterator.empty)(addressId => new AssetBalanceIterator(addressId, resource).asScala),
      includeAsset,
      diff.portfolios.getOrElse(address, Portfolio.empty).assets
    ).asScala
      .map(_.filter { case (asset, balance) =>
        includeAsset(asset) && balance > 0
      })
}
