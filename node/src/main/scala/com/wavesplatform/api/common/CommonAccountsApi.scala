package com.wavesplatform.api.common

import cats.instances.map._
import cats.syntax.monoid._
import com.google.common.base.Charsets
import com.google.common.collect.AbstractIterator
import com.google.common.primitives.{Ints, Longs, Shorts}
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common
import com.wavesplatform.database.{DBExt, Keys}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.Portfolio.longSemigroup
import com.wavesplatform.state.{Blockchain, DataEntry, Diff, Height, Portfolio}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import monix.eval.Task
import monix.reactive.Observable
import org.iq80.leveldb.DB

import scala.collection.JavaConverters._
import scala.collection.mutable

trait CommonAccountsApi {
  import CommonAccountsApi._

  def balance(address: Address, confirmations: Int = 0): Long

  def effectiveBalance(address: Address, confirmations: Int = 0): Long

  def balanceDetails(address: Address): BalanceDetails

  def assetBalance(address: Address, asset: IssuedAsset): Long

  def portfolio(address: Address): Map[IssuedAsset, Long]

  def nftPortfolio(address: Address, limit: Int, from: Option[IssuedAsset]): Observable[IssueTransaction]

  def script(address: Address): Option[(Script, Long)]

  def data(address: Address, key: String): Option[DataEntry[_]]

  def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]]

  def activeLeases(address: Address): Observable[(Height, LeaseTransaction)]

  def resolveAlias(alias: Alias): Either[ValidationError, Address]

  def wavesDistribution(height: Int): Observable[(Address, Long)]

  def assetDistribution(asset: IssuedAsset, height: Int): Observable[(Address, Long)]
}

object CommonAccountsApi {
  final case class BalanceDetails(regular: Long, generating: Long, available: Long, effective: Long, leaseIn: Long, leaseOut: Long)

  def apply(diff: => Diff, db: DB, blockchain: Blockchain): CommonAccountsApi = new CommonAccountsApi {

    override def balance(address: Address, confirmations: Int = 0): Long = {
      blockchain.balance(address, blockchain.height, confirmations)
    }

    override def effectiveBalance(address: Address, confirmations: Int = 0): Long = {
      blockchain.effectiveBalance(address, confirmations)
    }

    override def balanceDetails(address: Address): BalanceDetails = {
      val portfolio = blockchain.wavesPortfolio(address)
      BalanceDetails(
        portfolio.balance,
        blockchain.generatingBalance(address),
        portfolio.balance - portfolio.lease.out,
        portfolio.effectiveBalance,
        portfolio.lease.in,
        portfolio.lease.out
      )
    }

    override def assetBalance(address: Address, asset: IssuedAsset): Long = blockchain.balance(address, asset)

    override def portfolio(address: Address): Map[IssuedAsset, Long] =
      (diff.portfolios.getOrElse(address, Portfolio.empty).assets |+| common.portfolio(
        db,
        address,
        blockchain.isFeatureActivated(BlockchainFeatures.ReduceNFTFee)
      )).filter(_._2 > 0)

    override def nftPortfolio(address: Address, count: Int, from: Option[IssuedAsset]): Observable[IssueTransaction] =
      Observable.fromIterable(nftList(db, diff, blockchain.balance, address, count, from.map(_.id)))

    override def script(address: Address): Option[(Script, Long)] = blockchain.accountScript(address)

    override def data(address: Address, key: String): Option[DataEntry[_]] = {
      blockchain.accountData(address, key)
    }

    override def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]] = {
      val entriesFromDiff = diff.accountData.get(address).fold[Map[String, DataEntry[_]]](Map.empty)(_.data)
      val entries         = mutable.ArrayBuffer[DataEntry[_]](entriesFromDiff.values.toSeq: _*)

      db.readOnly { ro =>
        val addressId = db.get(Keys.addressId(address)).get
        db.iterateOver(Keys.DataHistoryPrefix) { e =>
          val key = new String(e.getKey.drop(2), Charsets.UTF_8)
          if (regex.forall(_.r.pattern.matcher(key).matches()) && !entriesFromDiff.contains(key)) {
            ro.get(Keys.data(addressId, key)(ro.get(Keys.dataHistory(addressId, key)).head)).foreach(entries += _)
          }
        }
      }
      Observable.fromIterable(entries)
    }

    override def resolveAlias(alias: Alias): Either[ValidationError, Address] = blockchain.resolveAlias(alias)

    override def activeLeases(address: Address): Observable[(Height, LeaseTransaction)] =
      Observable.fromIterable(Seq.empty)

    override def wavesDistribution(height: Int): Observable[(Address, Long)] = {
      val overrides = if (height == blockchain.height) diff.portfolios else Map.empty[Address, Portfolio]
      Observable
        .fromResource(db.resource)
        .flatMap { resource =>
          val wavesBalancePrefix = Shorts.toByteArray(Keys.WavesBalancePrefix)
          resource.iterator.seek(wavesBalancePrefix)
          Observable.fromIterator(Task(new AbstractIterator[(Address, Long)] {
            override def computeNext(): (Address, Long) =
              if (resource.iterator.hasNext && resource.iterator.peekNext().getKey.startsWith(wavesBalancePrefix)) {
                val current   = resource.iterator.next()
                val prefix    = current.getKey.dropRight(4)
                val addressId = prefix.drop(2)
                var balance   = Longs.fromByteArray(current.getValue)
                while (resource.iterator.hasNext && resource.iterator.peekNext().getKey.startsWith(prefix)) {
                  val next       = resource.iterator.next()
                  val nextHeight = Ints.fromByteArray(next.getKey.takeRight(4))
                  if (nextHeight <= height) {
                    balance = Longs.fromByteArray(next.getValue)
                  } else {}
                }

                val address = resource.get(Keys.idToAddress(BigInt(addressId)))
                address -> longSemigroup.combine(balance, overrides.getOrElse(address, Portfolio.empty).balance)
              } else endOfData()
          }.asScala))
        }
    }

    override def assetDistribution(asset: IssuedAsset, height: Int) = ???
  }
}
