package com.wavesplatform.api.common

import com.google.common.base.Charsets
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common
import com.wavesplatform.api.common.AddressPortfolio.{assetBalanceIterator, nftIterator}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, KeyTags, Keys}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, Blockchain, DataEntry, Diff, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.reactive.Observable
import org.iq80.leveldb.DB

import scala.collection.mutable

trait CommonAccountsApi {
  import CommonAccountsApi._

  def balance(address: Address, confirmations: Int = 0): Long

  def effectiveBalance(address: Address, confirmations: Int = 0): Long

  def balanceDetails(address: Address): BalanceDetails

  def assetBalance(address: Address, asset: IssuedAsset): Long

  def portfolio(address: Address): Observable[(IssuedAsset, Long)]

  def nftList(address: Address, after: Option[IssuedAsset]): Observable[(IssuedAsset, AssetDescription)]

  def script(address: Address): Option[AccountScriptInfo]

  def data(address: Address, key: String): Option[DataEntry[_]]

  def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]]

  def activeLeases(address: Address): Observable[(Height, LeaseTransaction)]

  def resolveAlias(alias: Alias): Either[ValidationError, Address]
}

object CommonAccountsApi extends ScorexLogging {
  def includeNft(blockchain: Blockchain)(assetId: IssuedAsset): Boolean =
    !blockchain.isFeatureActivated(BlockchainFeatures.ReduceNFTFee) || !blockchain.assetDescription(assetId).exists(_.nft)

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

    override def portfolio(address: Address): Observable[(IssuedAsset, Long)] =
      db.resourceObservable.flatMap { resource =>
        Observable.fromIterator(Task(assetBalanceIterator(resource, address, diff, includeNft(blockchain))))
      }

    override def nftList(address: Address, after: Option[IssuedAsset]): Observable[(IssuedAsset, AssetDescription)] =
      db.resourceObservable.flatMap { resource =>
        Observable.fromIterator(Task(nftIterator(resource, address, diff, after, blockchain.assetDescription)))
      }

    override def script(address: Address): Option[AccountScriptInfo] = blockchain.accountScript(address)

    override def data(address: Address, key: String): Option[DataEntry[_]] =
      blockchain.accountData(address, key)

    override def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]] = {
      val entriesFromDiff = diff.accountData
        .get(address)
        .fold[Map[String, DataEntry[_]]](Map.empty)(_.data.filter { case (k, _) => regex.forall(_.r.pattern.matcher(k).matches()) })
      val entries = mutable.ArrayBuffer[DataEntry[_]](entriesFromDiff.values.toSeq: _*)

      db.readOnly { ro =>
        db.get(Keys.addressId(address)).foreach { addressId =>
          db.iterateOver(KeyTags.DataHistory.prefixBytes ++ PBRecipients.publicKeyHash(address)) { e =>
            val key = new String(e.getKey.drop(2 + Address.HashLength), Charsets.UTF_8)
            if (regex.forall(_.r.pattern.matcher(key).matches()) && !entriesFromDiff.contains(key)) {
              for (h <- ro.get(Keys.dataHistory(address, key)).headOption; e <- ro.get(Keys.data(addressId, key)(h))) {
                entries += e
              }
            }
          }
        }
      }
      Observable.fromIterable(entries).filterNot(_.isEmpty)
    }

    override def resolveAlias(alias: Alias): Either[ValidationError, Address] = blockchain.resolveAlias(alias)

    override def activeLeases(address: Address): Observable[(Height, LeaseTransaction)] = {
      def leaseIsActive(id: ByteStr): Boolean = blockchain.leaseDetails(id).exists(_.isActive)
      common.activeLeases(db, Some(Height(blockchain.height) -> diff), address, leaseIsActive)
    }
  }
}
