package com.wavesplatform.api.common

import cats.instances.map._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.google.common.collect.AbstractIterator
import com.google.common.primitives.Shorts
import com.wavesplatform.account.Address
import com.wavesplatform.crypto
import com.wavesplatform.database.{DBExt, DBResource, Keys, readIntSeq}
import com.wavesplatform.state.Portfolio.longSemigroup
import com.wavesplatform.state.{AssetDescription, Diff}
import com.wavesplatform.transaction.Asset.IssuedAsset
import monix.eval.Task
import monix.reactive.Observable
import org.iq80.leveldb.DB

import scala.collection.JavaConverters._

trait AddressPortfolio {
  import AddressPortfolio._

  def nftList(
      db: DB,
      address: Address,
      diff: Diff,
      isNFT: IssuedAsset => Boolean,
      from: Option[IssuedAsset]
  ): Observable[(IssuedAsset, AssetDescription)] =
    db.resourceObservable.flatMap { resource =>
      Observable.fromIterator(Task(loadNftList(resource, address, diff, isNFT, from)))
    }

  def portfolio(db: DB, address: Address, overrides: Map[IssuedAsset, Long], includeAsset: IssuedAsset => Boolean): Observable[(IssuedAsset, Long)] =
    db.resourceObservable.flatMap { resource =>
      val maybeAddressId = resource.get(Keys.addressId(address))
      maybeAddressId.foreach { addressId =>
        resource.iterator.seek(Shorts.toByteArray(Keys.AssetBalanceHistoryPrefix) ++ addressId.toByteArray)
      }
      Observable
        .fromIterator(Task(new BalanceIterator(maybeAddressId, includeAsset, resource, overrides).asScala))
        .filter(_._2 != 0)

    }
}

object AddressPortfolio {
  def loadNftList(
      resource: DBResource,
      address: Address,
      diff: Diff,
      isNFT: IssuedAsset => Boolean,
      from: Option[IssuedAsset]
  ): Iterator[(IssuedAsset, AssetDescription)] = {
    val maybeAddressId = resource.get(Keys.addressId(address))

    maybeAddressId.foreach { addressId =>
      resource.iterator.seek(Shorts.toByteArray(Keys.AssetBalanceHistoryPrefix) ++ addressId.toByteArray ++ from.fold(Array.emptyByteArray)(_.id.arr))

      for (fromAssetId <- from if resource.iterator.hasNext && resource.iterator.peekNext().getKey.endsWith(fromAssetId.id.arr)) {
        resource.iterator.next()
      }
    }

    new BalanceIterator(maybeAddressId, isNFT, resource, diff.portfolios.get(address).map(_.assets).orEmpty).asScala
      .filter(_._2 != 0)
      .flatMap { case (assetId, _) => loadIssueTransaction(diff, resource, assetId).iterator }
  }

  private def loadIssueTransaction(diff: Diff, resource: DBResource, assetId: IssuedAsset): Option[(IssuedAsset, AssetDescription)] = ???

  class BalanceIterator(
      addressId: Option[BigInt],
      includeAsset: IssuedAsset => Boolean,
      resource: DBResource,
      private var pendingOverrides: Map[IssuedAsset, Long]
  ) extends AbstractIterator[(IssuedAsset, Long)] {

    private val prefix = addressId.map(aid => Shorts.toByteArray(Keys.AssetBalanceHistoryPrefix) ++ aid.toByteArray)
    private def stillSameAddress(k: Array[Byte]): Boolean = prefix.exists { p =>
      k.length == (p.length + crypto.DigestLength) && k.startsWith(p)
    }

    private def loadBalance(assetId: IssuedAsset, history: Seq[Int]): Long = {
      if (!includeAsset(assetId)) 0
      else {
        val balanceFromDiff = pendingOverrides.getOrElse(assetId, 0L)
        val balanceFromHistory = (for {
          id <- addressId
          h  <- history.headOption
        } yield resource.get(Keys.assetBalance(id, assetId)(h))).getOrElse(0L)
        balanceFromDiff |+| balanceFromHistory
      }
    }

    override def computeNext(): (IssuedAsset, Long) =
      if (resource.iterator.hasNext && stillSameAddress(resource.iterator.peekNext().getKey)) {
        val currentEntry = resource.iterator.next()
        val assetId      = IssuedAsset(currentEntry.getKey.takeRight(crypto.DigestLength))
        val balance      = loadBalance(assetId, readIntSeq(currentEntry.getValue))
        pendingOverrides -= assetId
        assetId -> balance
      } else if (pendingOverrides.nonEmpty) {
        val (asset, balance) = pendingOverrides.head
        val bool             = includeAsset(asset)
        val l                = if (bool) balance else 0
        pendingOverrides -= asset
        asset -> l
      } else endOfData()
  }
}
