package com.wavesplatform.api

import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, DBExt, Keys}
import com.wavesplatform.state.{Diff, Height, Portfolio, TxMeta}
import com.wavesplatform.transaction.{CreateAliasTransaction, Transaction, TransactionType}
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.RocksDB

import scala.jdk.CollectionConverters.*

package object common {
  import AddressTransactions.*
  import BalanceDistribution.*

  def addressTransactions(
      db: RocksDB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[TransactionMeta] =
    allAddressTransactions(db, maybeDiff, subject, sender, types, fromId).map { case (m, transaction, txNumOpt) =>
      def loadISR(t: Transaction) =
        maybeDiff
          .flatMap { case (_, diff) => diff.scriptResults.get(t.id()) }
          .orElse(txNumOpt.flatMap(loadInvokeScriptResult(db, m.height, _)))

      def loadETM(t: Transaction) =
        maybeDiff
          .flatMap { case (_, diff) => diff.ethereumTransactionMeta.get(t.id()) }
          .orElse(txNumOpt.flatMap(loadEthereumMetadata(db, m.height, _)))

      TransactionMeta.create(
        m.height,
        transaction,
        m.succeeded,
        m.spentComplexity,
        loadISR,
        loadETM
      )
    }

  def balanceDistribution(
      db: RocksDB,
      height: Int,
      after: Option[Address],
      overrides: Map[Address, Portfolio],
      globalPrefix: Array[Byte],
      addressId: Array[Byte] => AddressId,
      balanceOf: Portfolio => Long
  ): Observable[(Address, Long)] =
    db.resourceObservable
      .flatMap { resource =>
        resource.fullIterator.seek(
          globalPrefix ++ after
            .flatMap(address => resource.get(Keys.addressId(address)))
            .fold(Array.emptyByteArray)(id => Longs.toByteArray(id.toLong + 1))
        )
        Observable.fromIterator(Task(new BalanceIterator(resource, globalPrefix, addressId, balanceOf, height, overrides).asScala.filter(_._2 > 0)))
      }

  def aliasesOfAddress(db: RocksDB, maybeDiff: => Option[(Height, Diff)], address: Address): Observable[(Height, CreateAliasTransaction)] = {
    val disabledAliases = db.get(Keys.disabledAliases)
    addressTransactions(db, maybeDiff, address, Some(address), Set(TransactionType.CreateAlias), None)
      .collect {
        case TransactionMeta(height, cat: CreateAliasTransaction, true) if disabledAliases.isEmpty || !disabledAliases(cat.alias) => height -> cat
      }
  }

  def loadTransactionMeta(db: RocksDB, maybeDiff: => Option[(Int, Diff)])(tuple: (TxMeta, Transaction)): TransactionMeta = {
    val (meta, transaction) = tuple
    TransactionMeta.create(
      meta.height,
      transaction,
      meta.succeeded,
      meta.spentComplexity,
      ist =>
        maybeDiff
          .flatMap { case (_, diff) => diff.scriptResults.get(ist.id()) }
          .orElse(loadInvokeScriptResult(db, ist.id())),
      et =>
        maybeDiff
          .flatMap { case (_, diff) => diff.ethereumTransactionMeta.get(et.id()) }
          .orElse(loadEthereumMetadata(db, et.id()))
    )
  }
}
