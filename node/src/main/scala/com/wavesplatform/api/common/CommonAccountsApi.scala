package com.wavesplatform.api.common

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common
import com.wavesplatform.api.common.AddressPortfolio.{assetBalanceIterator, nftIterator}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database
import com.wavesplatform.database.{DBExt, KeyTags, Keys}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, Blockchain, DataEntry, Diff, Height}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.utils.{DebugUtils, ScorexLogging}
import monix.eval.Task
import monix.reactive.Observable
import org.iq80.leveldb.DB

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

    override def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]] = Observable.defer {
      val pattern = regex.map(_.r.pattern)
      val entriesFromDiff = diff.accountData
        .get(address)
        .fold[Map[String, DataEntry[_]]](Map.empty)(_.data.filter { case (k, _) => pattern.forall(_.matcher(k).matches()) })

      val baseName    = s"AccountData[$address, ${regex.getOrElse(".*")}]"
      val iterate     = DebugUtils.startMulti(s"$baseName iterateOver")
      val readHistory = DebugUtils.startMulti(s"$baseName read history")
      val readValues  = DebugUtils.startMulti(s"$baseName read values")
      val entries = db.readOnly { ro =>
        db.get(Keys.addressId(address)).fold(Seq.empty[DataEntry[_]]) { addressId =>
          var start        = iterate.startOperation()
          val filteredKeys = Set.newBuilder[String]

          db.iterateOver(KeyTags.ChangedDataKeys.prefixBytes ++ addressId.toByteArray) { e =>
            val keys = database.readStrings(e.getValue)
            iterate.finishOperation(start)

            for (key <- keys if !entriesFromDiff.contains(key) && pattern.forall(_.matcher(key).matches()))
              filteredKeys += key

            start = iterate.startOperation()
          }

          for {
            key <- filteredKeys.result().toVector
            h   <- readHistory.measureOperation(ro.get(Keys.dataHistory(address, key)).headOption)
            e   <- readValues.measureOperation(ro.get(Keys.data(addressId, key)(h)))
          } yield e
        }
      }
      log.info(iterate.toString)
      log.info(readHistory.toString)
      log.info(readValues.toString)
      Observable.fromIterable(entries.filterNot(_.isEmpty))
    }

    override def resolveAlias(alias: Alias): Either[ValidationError, Address] = blockchain.resolveAlias(alias)

    override def activeLeases(address: Address): Observable[(Height, LeaseTransaction)] = {
      def leaseIsActive(id: ByteStr): Boolean = blockchain.leaseDetails(id).exists(_.isActive)
      common.activeLeases(db, Some(Height(blockchain.height) -> diff), address, leaseIsActive)
    }
  }
}
