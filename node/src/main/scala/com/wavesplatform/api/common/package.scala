package com.wavesplatform.api

import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, DBExt, Keys, RDB}
import com.wavesplatform.state.{Height, StateSnapshot, TxMeta}
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, Transaction, TransactionType}
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.RocksDB

import scala.jdk.CollectionConverters.*

package object common {
  import AddressTransactions.*
  import BalanceDistribution.*

  def addressTransactions(
      rdb: RDB,
      maybeDiff: Option[(Height, StateSnapshot)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[TransactionMeta] =
    allAddressTransactions(rdb, maybeDiff, subject, sender, types, fromId).map { case (m, transaction, txNumOpt) =>
      def loadISR(t: Transaction) =
        maybeDiff
          .flatMap { case (_, diff) => diff.scriptResults.get(t.id()) }
          .orElse(txNumOpt.flatMap(loadInvokeScriptResult(rdb.db, rdb.apiHandle, m.height, _)))

      def loadETM(t: Transaction) =
        maybeDiff
          .flatMap { case (_, diff) => diff.ethereumTransactionMeta.get(t.id()) }
          .orElse(txNumOpt.flatMap(loadEthereumMetadata(rdb.db, rdb.apiHandle, m.height, _)))

      TransactionMeta.create(
        m.height,
        transaction,
        m.status,
        m.spentComplexity,
        loadISR,
        loadETM
      )
    }

  def balanceDistribution(
      db: RocksDB,
      height: Int,
      after: Option[Address],
      overrides: Map[(Address, Asset), Long],
      globalPrefix: Array[Byte],
      addressId: Array[Byte] => AddressId,
      asset: Asset
  ): Observable[(Address, Long)] =
    db.resourceObservable
      .flatMap { resource =>
        resource.fullIterator.seek(
          globalPrefix ++ after
            .flatMap(address => resource.get(Keys.addressId(address)))
            .fold(Array.emptyByteArray)(id => Longs.toByteArray(id.toLong + 1))
        )
        Observable.fromIterator(Task(new BalanceIterator(resource, globalPrefix, addressId, asset, height, overrides).asScala.filter(_._2 > 0)))
      }

  def aliasesOfAddress(
      rdb: RDB,
      maybeDiff: => Option[(Height, StateSnapshot)],
      address: Address
  ): Observable[(Height, CreateAliasTransaction)] = {
    val disabledAliases = rdb.db.get(Keys.disabledAliases)
    addressTransactions(rdb, maybeDiff, address, Some(address), Set(TransactionType.CreateAlias), None)
      .collect {
        case TransactionMeta(height, cat: CreateAliasTransaction, TxMeta.Status.Succeeded)
            if disabledAliases.isEmpty || !disabledAliases(cat.alias) =>
          height -> cat
      }
  }

  def loadTransactionMeta(rdb: RDB, maybeSnapshot: => Option[(Int, StateSnapshot)])(
      tuple: (TxMeta, Transaction)
  ): TransactionMeta = {
    val (meta, transaction) = tuple
    TransactionMeta.create(
      meta.height,
      transaction,
      meta.status,
      meta.spentComplexity,
      ist =>
        maybeSnapshot
          .flatMap { case (_, s) => s.scriptResults.get(ist.id()) }
          .orElse(loadInvokeScriptResult(rdb.db, rdb.txMetaHandle, rdb.apiHandle, ist.id())),
      et =>
        maybeSnapshot
          .flatMap { case (_, s) => s.ethereumTransactionMeta.get(et.id()) }
          .orElse(loadEthereumMetadata(rdb.db, rdb.txMetaHandle, rdb.apiHandle, et.id()))
    )
  }
}
