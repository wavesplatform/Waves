package com.wavesplatform.api.common

import com.google.common.base.Charsets
import com.google.common.collect.AbstractIterator
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common.AddressPortfolio.{assetBalanceIterator, nftIterator}
import com.wavesplatform.api.common.CommonAccountsApi.AddressDataIterator.BatchSize
import com.wavesplatform.api.common.TransactionMeta.Ethereum
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.{AddressId, DBExt, DBResource, KeyTags, Keys, readIntSeq}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.PBRecipients
import com.wavesplatform.state.patch.CancelLeasesToDisabledAliases
import com.wavesplatform.state.reader.LeaseDetails.Status
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, Blockchain, DataEntry, Diff, Height, InvokeScriptResult}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.EthereumTransaction.Invocation
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.{EthereumTransaction, TransactionType}
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

trait CommonAccountsApi {
  import CommonAccountsApi.*

  def balance(address: Address, confirmations: Int = 0): Long

  def effectiveBalance(address: Address, confirmations: Int = 0): Long

  def balanceDetails(address: Address): Either[String, BalanceDetails]

  def assetBalance(address: Address, asset: IssuedAsset): Long

  def portfolio(address: Address): Observable[(IssuedAsset, Long)]

  def nftList(address: Address, after: Option[IssuedAsset]): Observable[(IssuedAsset, AssetDescription)]

  def script(address: Address): Option[AccountScriptInfo]

  def data(address: Address, key: String): Option[DataEntry[?]]

  def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[?]]

  def activeLeases(address: Address): Observable[LeaseInfo]

  def leaseInfo(leaseId: ByteStr): Option[LeaseInfo]

  def resolveAlias(alias: Alias): Either[ValidationError, Address]
}

object CommonAccountsApi {
  def includeNft(blockchain: Blockchain)(assetId: IssuedAsset): Boolean =
    !blockchain.isFeatureActivated(BlockchainFeatures.ReduceNFTFee) || !blockchain.assetDescription(assetId).exists(_.nft)

  final case class BalanceDetails(regular: Long, generating: Long, available: Long, effective: Long, leaseIn: Long, leaseOut: Long)

  def apply(diff: () => Diff, db: RocksDB, blockchain: Blockchain): CommonAccountsApi = new CommonAccountsApi {

    override def balance(address: Address, confirmations: Int = 0): Long = {
      blockchain.balance(address, blockchain.height, confirmations)
    }

    override def effectiveBalance(address: Address, confirmations: Int = 0): Long = {
      blockchain.effectiveBalance(address, confirmations)
    }

    override def balanceDetails(address: Address): Either[String, BalanceDetails] = {
      val portfolio = blockchain.wavesPortfolio(address)
      portfolio.effectiveBalance.map(effectiveBalance =>
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

    override def portfolio(address: Address): Observable[(IssuedAsset, Long)] = {
      val currentDiff = diff()
      db.resourceObservable.flatMap { resource =>
        Observable
          .fromIterator(Task(assetBalanceIterator(resource, address, currentDiff, includeNft(blockchain))))
          .concatMapIterable(identity)
      }
    }

    override def nftList(address: Address, after: Option[IssuedAsset]): Observable[(IssuedAsset, AssetDescription)] = {
      val currentDiff = diff()
      db.resourceObservable.flatMap { resource =>
        Observable
          .fromIterator(Task(nftIterator(resource, address, currentDiff, after, blockchain.assetDescription)))
          .concatMapIterable(identity)
      }
    }

    override def script(address: Address): Option[AccountScriptInfo] = blockchain.accountScript(address)

    override def data(address: Address, key: String): Option[DataEntry[?]] =
      blockchain.accountData(address, key)

    override def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[?]] = Observable.defer {
      val pattern = regex.map(_.r.pattern)
      val entriesFromDiff = diff().accountData
        .get(address)
        .fold(Array.empty[DataEntry[?]])(_.data.filter { case (k, _) => pattern.forall(_.matcher(k).matches()) }.values.toArray.sortBy(_.key))

      db.resourceObservable.flatMap { dbResource =>
        dbResource
          .get(Keys.addressId(address))
          .map { addressId =>
            Observable.fromIterator(
              Task(new AddressDataIterator(dbResource, address, addressId, entriesFromDiff, pattern).asScala)
            )
          }
          .getOrElse(Observable.empty)
          .filterNot(_.isEmpty)
      }
    }

    override def resolveAlias(alias: Alias): Either[ValidationError, Address] = blockchain.resolveAlias(alias)

    override def activeLeases(address: Address): Observable[LeaseInfo] =
      addressTransactions(
        db,
        Some(Height(blockchain.height) -> diff()),
        address,
        None,
        Set(TransactionType.Lease, TransactionType.InvokeScript, TransactionType.InvokeExpression, TransactionType.Ethereum),
        None
      ).flatMapIterable {
        case TransactionMeta(leaseHeight, lt: LeaseTransaction, true) if leaseIsActive(lt.id()) =>
          Seq(
            LeaseInfo(
              lt.id(),
              lt.id(),
              lt.sender.toAddress,
              blockchain.resolveAlias(lt.recipient).explicitGet(),
              lt.amount.value,
              leaseHeight,
              LeaseInfo.Status.Active
            )
          )
        case TransactionMeta.Invoke(invokeHeight, originTransaction, true, _, Some(scriptResult)) =>
          extractLeases(address, scriptResult, originTransaction.id(), invokeHeight)
        case Ethereum(height, tx @ EthereumTransaction(_: Invocation, _, _, _), true, _, _, Some(scriptResult)) =>
          extractLeases(address, scriptResult, tx.id(), height)
        case _ => Seq()
      }

    private def extractLeases(subject: Address, result: InvokeScriptResult, txId: ByteStr, height: Height): Seq[LeaseInfo] = {
      (for {
        lease   <- result.leases
        details <- blockchain.leaseDetails(lease.id) if details.isActive
        sender = details.sender.toAddress
        recipient <- blockchain.resolveAlias(lease.recipient).toOption if subject == sender || subject == recipient
      } yield LeaseInfo(
        lease.id,
        txId,
        sender,
        recipient,
        lease.amount,
        height,
        LeaseInfo.Status.Active
      )) ++ {
        result.invokes.flatMap(i => extractLeases(subject, i.stateChanges, txId, height))
      }
    }

    private def resolveDisabledAlias(leaseId: ByteStr): Either[ValidationError, Address] =
      CancelLeasesToDisabledAliases.patchData
        .get(leaseId)
        .fold[Either[ValidationError, Address]](Left(GenericError("Unknown lease ID"))) { case (_, recipientAddress) =>
          Right(recipientAddress)
        }

    def leaseInfo(leaseId: ByteStr): Option[LeaseInfo] = blockchain.leaseDetails(leaseId) map { ld =>
      LeaseInfo(
        leaseId,
        ld.sourceId,
        ld.sender.toAddress,
        blockchain.resolveAlias(ld.recipient).orElse(resolveDisabledAlias(leaseId)).explicitGet(),
        ld.amount,
        ld.height,
        ld.status match {
          case Status.Active          => LeaseInfo.Status.Active
          case Status.Cancelled(_, _) => LeaseInfo.Status.Canceled
          case Status.Expired(_)      => LeaseInfo.Status.Expired
        },
        ld.status.cancelHeight,
        ld.status.cancelTransactionId
      )
    }

    private[this] def leaseIsActive(id: ByteStr): Boolean =
      blockchain.leaseDetails(id).exists(_.isActive)
  }

  class AddressDataIterator(
      db: DBResource,
      address: Address,
      addressId: AddressId,
      entriesFromDiff: Array[DataEntry[?]],
      pattern: Option[Pattern]
  ) extends AbstractIterator[DataEntry[?]] {
    val prefix: Array[Byte] = KeyTags.DataHistory.prefixBytes ++ PBRecipients.publicKeyHash(address)

    val length: Int = entriesFromDiff.length

    db.withSafePrefixIterator(_.seek(prefix))()

    var nextIndex                        = 0
    var nextDbEntries: Seq[DataEntry[?]] = Seq.empty

    def matches(key: String): Boolean = pattern.forall(_.matcher(key).matches())

    final override def computeNext(): DataEntry[?] = db.withSafePrefixIterator { dbIterator =>
      nextDbEntries.headOption match {
        case Some(dbEntry) =>
          if (nextIndex < length) {
            val entryFromDiff = entriesFromDiff(nextIndex)
            if (entryFromDiff.key < dbEntry.key) {
              nextIndex += 1
              entryFromDiff
            } else if (entryFromDiff.key == dbEntry.key) {
              nextIndex += 1
              nextDbEntries = nextDbEntries.tail
              entryFromDiff
            } else {
              nextDbEntries = nextDbEntries.tail
              dbEntry
            }
          } else {
            nextDbEntries = nextDbEntries.tail
            dbEntry
          }
        case None =>
          val buffer = new ArrayBuffer[(String, Int)]()
          while (dbIterator.isValid && buffer.length < BatchSize) {
            val key = new String(dbIterator.key().drop(2 + Address.HashLength), Charsets.UTF_8)
            if (matches(key)) {
              readIntSeq(dbIterator.value()).headOption match {
                case Some(h) => buffer.addOne(key -> h)
                case None    => ()
              }
            }
            dbIterator.next()
          }
          if (buffer.nonEmpty) {
            nextDbEntries = db.multiGetFlat(buffer.map { case (key, h) => Keys.data(addressId, key)(h) })
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

  object AddressDataIterator {
    val BatchSize = 1000
  }
}
