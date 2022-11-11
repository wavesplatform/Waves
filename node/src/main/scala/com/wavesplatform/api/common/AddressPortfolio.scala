package com.wavesplatform.api.common

import cats.syntax.semigroup.*
import com.google.common.collect.AbstractIterator
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.NFTIterator.BatchSize
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.database.{AddressId, DBResource, KeyTags, Keys}
import com.wavesplatform.state.{AssetDescription, Diff, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.ScorexLogging

import java.nio.ByteBuffer
import scala.annotation.tailrec
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
    }
  }(())

  override def computeNext(): Seq[(IssuedAsset, Long)] = resource.withSafePrefixIterator { dbIterator =>
    val keysBuffer   = new ArrayBuffer[Array[Byte]]()
    val assetsBuffer = new ArrayBuffer[IssuedAsset]()
    while (dbIterator.isValid && keysBuffer.length < BatchSize) {
      val assetId = IssuedAsset(ByteStr(dbIterator.key().takeRight(crypto.DigestLength)))
      keysBuffer.addOne(Keys.assetBalanceHistory(addressId, assetId).keyBytes)
      assetsBuffer.addOne(assetId)
      dbIterator.next()
    }
    if (keysBuffer.nonEmpty) {
      val assetBalanceKeys = resource
        .multiGetInts(keysBuffer)
        .zip(assetsBuffer)
        .map { case (heightOpt, assetId) =>
          Keys.assetBalance(addressId, assetId)(heightOpt.getOrElse(0)).keyBytes
        }
        .toSeq
      resource
        .multiGetLongs(assetBalanceKeys)
        .zip(assetsBuffer)
        .map(_.swap)
        .toSeq
    } else endOfData()
  }(endOfData())
}

object NFTIterator {
  val BatchSize = 1000
}

class AssetBalanceIterator(addressId: AddressId, resource: DBResource) extends AbstractIterator[Seq[(IssuedAsset, Long)]] {
  private val prefixBytes: Array[Byte] = KeyTags.AssetBalanceHistory.prefixBytes ++ addressId.toByteArray

  resource.withSafePrefixIterator(_.seek(prefixBytes))(())

  private def stillSameAddress(k: Array[Byte]): Boolean =
    k.length == (prefixBytes.length + crypto.DigestLength)

  override def computeNext(): Seq[(IssuedAsset, Long)] = resource.withSafePrefixIterator { dbIterator =>
    val keysBuffer   = new ArrayBuffer[Array[Byte]]()
    val assetsBuffer = new ArrayBuffer[IssuedAsset]()

    @tailrec
    def loop(): Unit = {
      if (dbIterator.isValid) {
        val key = dbIterator.key()
        if (stillSameAddress(key) && keysBuffer.length < BatchSize) {
          val assetId = IssuedAsset(ByteStr(key.takeRight(crypto.DigestLength)))
          val history = Option(dbIterator.value()).fold(0)(arr => ByteBuffer.wrap(arr).getInt) // FIXME: refactor
          keysBuffer.addOne(Keys.assetBalance(addressId, assetId)(history).keyBytes)
          assetsBuffer.addOne(assetId)
          dbIterator.next()
          loop()
        } else ()
      } else ()
    }

    loop()
    if (keysBuffer.nonEmpty) {
      resource
        .multiGetLongs(keysBuffer)
        .zip(assetsBuffer)
        .map(_.swap)
        .toSeq
    } else endOfData()
  }(endOfData())
}

object AssetBalanceIterator {
  val BatchSize = 100
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
        .fold[Iterator[Seq[(IssuedAsset, Long)]]](Iterator(Seq.empty))(addressId => new NFTIterator(addressId, maybeAfter, resource).asScala),
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
        .fold[Iterator[Seq[(IssuedAsset, Long)]]](Iterator(Seq.empty))(addressId => new AssetBalanceIterator(addressId, resource).asScala),
      includeAsset,
      diff.portfolios.getOrElse(address, Portfolio.empty).assets
    ).asScala
      .map(_.filter { case (asset, balance) =>
        includeAsset(asset) && balance > 0
      })

}
