package com.wavesplatform.api.common

import cats.instances.map._
import cats.syntax.monoid._
import com.google.common.base.Charsets
import com.google.common.primitives.Shorts
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common
import com.wavesplatform.crypto
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
import com.wavesplatform.utils.ScorexLogging
import monix.reactive.Observable
import org.iq80.leveldb.DB

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

  def wavesDistribution(height: Int, after: Option[Address]): Observable[(Address, Long)]

  def assetDistribution(asset: IssuedAsset, height: Int, after: Option[Address]): Observable[(Address, Long)]
}

object CommonAccountsApi extends ScorexLogging {
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

    override def data(address: Address, key: String): Option[DataEntry[_]] =
      blockchain.accountData(address, key)

    override def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]] = {
      val entriesFromDiff = diff.accountData.get(address).fold[Map[String, DataEntry[_]]](Map.empty)(_.data)
      val entries         = mutable.ArrayBuffer[DataEntry[_]](entriesFromDiff.values.toSeq: _*)

      db.readOnly { ro =>
        val addressId = db.get(Keys.addressId(address)).get
        db.iterateOver(Shorts.toByteArray(Keys.DataHistoryPrefix) ++ addressId.toByteArray) { e =>
          val key = new String(e.getKey.drop(2 + addressId.toByteArray.length), Charsets.UTF_8)
          if (regex.forall(_.r.pattern.matcher(key).matches()) && !entriesFromDiff.contains(key)) {
            for (h <- ro.get(Keys.dataHistory(addressId, key)).headOption; e <- ro.get(Keys.data(addressId, key)(h))) {
              entries += e
            }
          }
        }
      }
      Observable.fromIterable(entries)
    }

    override def resolveAlias(alias: Alias): Either[ValidationError, Address] = blockchain.resolveAlias(alias)

    override def activeLeases(address: Address): Observable[(Height, LeaseTransaction)] =
      Observable.fromIterable(Seq.empty)

    override def wavesDistribution(height: Int, after: Option[Address]): Observable[(Address, Long)] =
      balanceDistribution(
        db,
        height,
        after,
        if (height == blockchain.height) diff.portfolios else Map.empty[Address, Portfolio],
        Shorts.toByteArray(Keys.WavesBalancePrefix),
        bs => BigInt(bs.slice(2, bs.length - 4)),
        _.balance
      )

    override def assetDistribution(asset: IssuedAsset, height: Int, after: Option[Address]): Observable[(Address, Long)] =
      balanceDistribution(
        db,
        height,
        after,
        if (height == blockchain.height) diff.portfolios else Map.empty[Address, Portfolio],
        Shorts.toByteArray(Keys.AssetBalancePrefix) ++ asset.id.arr,
        bs => BigInt(bs.slice(2 + crypto.DigestLength, bs.length - 4)),
        _.assets.getOrElse(asset, 0L)
      )
  }
}
