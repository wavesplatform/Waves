package com.wavesplatform.api.common

import com.google.common.base.Charsets
import com.google.common.collect.AbstractIterator
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common.AddressPortfolio.{assetBalanceIterator, nftIterator}
import com.wavesplatform.api.common.lease.{AddressLeaseInfo, OldAddressLeaseInfo}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, DBResource, KeyTags, Keys, RDB}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, Blockchain, DataEntry}
import com.wavesplatform.transaction.Asset.IssuedAsset
import monix.eval.Task
import monix.reactive.Observable

import java.util.regex.Pattern
import scala.jdk.CollectionConverters.*

trait CommonAccountsApi {
  import CommonAccountsApi.*

  def balance(address: Address, confirmations: Int = 0): Long

  def effectiveBalance(address: Address, confirmations: Int = 0): Long

  def balanceDetails(address: Address): Either[String, BalanceDetails]

  def assetBalance(address: Address, asset: IssuedAsset): Long

  def portfolio(address: Address): Observable[Seq[(IssuedAsset, Long)]]

  def nftList(address: Address, after: Option[IssuedAsset]): Observable[Seq[(IssuedAsset, AssetDescription)]]

  def script(address: Address): Option[AccountScriptInfo]

  def data(address: Address, key: String): Option[DataEntry[?]]

  def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[?]]

  def activeLeases(address: Address): Observable[LeaseInfo]

  def leaseInfo(leaseId: ByteStr): Option[LeaseInfo]

  def resolveAlias(alias: Alias): Either[ValidationError, Address]
}

object CommonAccountsApi {
  final case class BalanceDetails(regular: Long, generating: Long, available: Long, effective: Long, leaseIn: Long, leaseOut: Long)

  def apply(
      compositeBlockchain: () => SnapshotBlockchain,
      rdb: RDB,
      blockchain: Blockchain,
      leaseStatesAreStoredByAddress: Boolean
  ): CommonAccountsApi = new CommonAccountsApi {

    override def balance(address: Address, confirmations: Int = 0): Long =
      blockchain.balance(address, blockchain.height, confirmations)

    override def effectiveBalance(address: Address, confirmations: Int = 0): Long = {
      blockchain.effectiveBalance(address, confirmations)
    }

    override def balanceDetails(address: Address): Either[String, BalanceDetails] = {
      val portfolio = blockchain.wavesPortfolio(address)
      val isBanned  = blockchain.hasBannedEffectiveBalance(address)
      portfolio
        .effectiveBalance(isBanned)
        .map(effectiveBalance =>
          BalanceDetails(
            portfolio.balance,
            blockchain.generatingBalance(address),
            portfolio.balance - portfolio.lease.out,
            effectiveBalance,
            portfolio.lease.in,
            portfolio.lease.out
          )
        )
    }

    override def assetBalance(address: Address, asset: IssuedAsset): Long = blockchain.balance(address, asset)

    override def portfolio(address: Address): Observable[Seq[(IssuedAsset, Long)]] = {
      val featureNotActivated = !blockchain.isFeatureActivated(BlockchainFeatures.ReduceNFTFee)
      val compBlockchain      = compositeBlockchain()
      def includeNft(assetId: IssuedAsset): Boolean =
        featureNotActivated || !compBlockchain.assetDescription(assetId).exists(_.nft)

      rdb.db.resourceObservable.flatMap { resource =>
        Observable.fromIterator(Task(assetBalanceIterator(resource, address, compBlockchain.snapshot, includeNft)))
      }
    }

    override def nftList(address: Address, after: Option[IssuedAsset]): Observable[Seq[(IssuedAsset, AssetDescription)]] = {
      rdb.db.resourceObservable.flatMap { resource =>
        Observable
          .fromIterator(Task(nftIterator(resource, address, compositeBlockchain().snapshot, after, blockchain.assetDescription)))
      }
    }

    override def script(address: Address): Option[AccountScriptInfo] = blockchain.accountScript(address)

    override def data(address: Address, key: String): Option[DataEntry[?]] =
      blockchain.accountData(address, key)

    override def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[?]] = Observable.defer {
      val pattern = regex.map(_.r.pattern)
      val entriesFromDiff = compositeBlockchain().snapshot.accountData
        .get(address)
        .fold(Array.empty[DataEntry[?]])(_.filter { case (k, _) => pattern.forall(_.matcher(k).matches()) }.values.toArray.sortBy(_.key))

      rdb.db.resourceObservable.flatMap { dbResource =>
        Observable
          .fromIterator(
            Task(new AddressDataIterator(dbResource, address, entriesFromDiff, pattern).asScala)
          )
          .filterNot(_.isEmpty)
      }
    }

    override def resolveAlias(alias: Alias): Either[ValidationError, Address] = blockchain.resolveAlias(alias)

    override def activeLeases(address: Address): Observable[LeaseInfo] =
      if (leaseStatesAreStoredByAddress)
        AddressLeaseInfo.activeLeases(rdb, compositeBlockchain().snapshot, blockchain, address)
      else
        OldAddressLeaseInfo.activeLeases(rdb, compositeBlockchain().snapshot, blockchain, address)

    def leaseInfo(leaseId: ByteStr): Option[LeaseInfo] =
      blockchain.leaseDetails(leaseId).map(LeaseInfo.fromLeaseDetails(leaseId, _, blockchain))
  }

  private class AddressDataIterator(
      db: DBResource,
      address: Address,
      entriesFromDiff: Array[DataEntry[?]],
      pattern: Option[Pattern]
  ) extends AbstractIterator[DataEntry[?]] {
    val prefix: Array[Byte] = KeyTags.Data.prefixBytes ++ PBRecipients.publicKeyHash(address)

    val length: Int = entriesFromDiff.length

    db.withSafePrefixIterator(_.seek(prefix))()

    var nextIndex                         = 0
    var nextDbEntry: Option[DataEntry[?]] = None

    def matches(key: String): Boolean = pattern.forall(_.matcher(key).matches())

    final override def computeNext(): DataEntry[?] = db.withSafePrefixIterator { dbIterator =>
      nextDbEntry match {
        case Some(dbEntry) =>
          if (nextIndex < length) {
            val entryFromDiff = entriesFromDiff(nextIndex)
            if (entryFromDiff.key < dbEntry.key) {
              nextIndex += 1
              entryFromDiff
            } else if (entryFromDiff.key == dbEntry.key) {
              nextIndex += 1
              nextDbEntry = None
              entryFromDiff
            } else {
              nextDbEntry = None
              dbEntry
            }
          } else {
            nextDbEntry = None
            dbEntry
          }
        case None =>
          if (dbIterator.isValid) {
            val key = new String(dbIterator.key().drop(2 + Address.HashLength), Charsets.UTF_8)
            if (matches(key)) {
              nextDbEntry = Option(dbIterator.value()).map { arr =>
                Keys.data(address, key).parse(arr).entry
              }
            }
            dbIterator.next()
            computeNext()
          } else if (nextIndex < length) {
            nextIndex += 1
            entriesFromDiff(nextIndex - 1)
          } else {
            endOfData()
          }
      }
    }(endOfData())
  }
}
