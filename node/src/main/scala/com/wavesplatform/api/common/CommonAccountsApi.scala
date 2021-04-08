package com.wavesplatform.api.common

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common
import com.wavesplatform.api.common.AddressPortfolio.{assetBalanceIterator, nftIterator}
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database
import com.wavesplatform.database.{DBExt, Keys, KeyTags}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, Blockchain, DataEntry, Diff, Height}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.utils.ScorexLogging
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

  def activeLeasesOld(address: Address): Observable[(Height, LeaseTransaction)]

  def activeLeases(address: Address): Observable[LeaseInfo]

  def leaseInfo(leaseId: ByteStr): Option[LeaseInfo]

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

      val entries = db.readOnly { ro =>
        ro.get(Keys.addressId(address)).fold(Seq.empty[DataEntry[_]]) { addressId =>
          val filteredKeys = Set.newBuilder[String]

          ro.iterateOver(KeyTags.ChangedDataKeys.prefixBytes ++ addressId.toByteArray) { e =>
            for (key <- database.readStrings(e.getValue) if !entriesFromDiff.contains(key) && pattern.forall(_.matcher(key).matches()))
              filteredKeys += key
          }

          for {
            key <- filteredKeys.result().toVector
            h   <- ro.get(Keys.dataHistory(address, key)).headOption
            e   <- ro.get(Keys.data(addressId, key)(h))
          } yield e
        }
      }
      Observable.fromIterable((entriesFromDiff.values ++ entries).filterNot(_.isEmpty))
    }

    override def resolveAlias(alias: Alias): Either[ValidationError, Address] = blockchain.resolveAlias(alias)

    override def activeLeasesOld(address: Address): Observable[(Height, LeaseTransaction)] = {
      common.activeLeases(db, Some(Height(blockchain.height) -> diff), address, leaseIsActive)
    }

    override def activeLeases(address: Address): Observable[LeaseInfo] = {
      addressTransactions(
        db,
        Some(Height(blockchain.height) -> diff),
        address,
        None,
        Set(InvokeScriptTransaction.typeId, LeaseTransaction.typeId),
        None
      ).flatMapIterable {
        case TransactionMeta(height, lt: LeaseTransaction, true) if leaseIsActive(lt.id()) =>
          val recipient = blockchain.resolveAlias(lt.recipient).explicitGet()
          Seq(
            LeaseInfo(lt.id(), lt.id(), lt.sender.toAddress, recipient, lt.amount, height, LeaseInfo.Status.active)
          )

        case TransactionMeta.Invoke(height, invoke, true, scriptResult) =>
          scriptResult.toSeq
            .flatMap(_.leases.filter(l => leaseIsActive(l.leaseId)))
            .map { lease =>
              val sender    = blockchain.resolveAlias(invoke.dAppAddressOrAlias).explicitGet()
              val recipient = blockchain.resolveAlias(lease.recipient).explicitGet()
              LeaseInfo(lease.leaseId, invoke.id(), sender, recipient, lease.amount, height, LeaseInfo.Status.active)
            }
        case _ => Seq()
      }
    }

    def leaseInfo(leaseId: ByteStr): Option[LeaseInfo] = blockchain.leaseDetails(leaseId) map { ld =>
      val (height, _) = blockchain.transactionMeta(ld.sourceId).get

      LeaseInfo(
        leaseId,
        ld.sourceId,
        ld.sender.toAddress,
        blockchain.resolveAlias(ld.recipient).explicitGet(),
        ld.amount,
        height,
        if (ld.isActive) LeaseInfo.Status.active else LeaseInfo.Status.canceled,
        LeaseDetails.Status.getCancelHeight(ld.status),
        LeaseDetails.Status.getCancelTransactionId(ld.status)
      )
    }

    private[this] def leaseIsActive(id: ByteStr): Boolean =
      blockchain.leaseDetails(id).exists(_.isActive)
  }

}
