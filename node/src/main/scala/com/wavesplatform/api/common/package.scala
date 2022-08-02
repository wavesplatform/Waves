package com.wavesplatform.api

import com.google.common.primitives.Longs
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{AddressId, DBExt, Keys}
import com.wavesplatform.state.{Diff, Height, Portfolio, TxMeta}
import com.wavesplatform.transaction.{CreateAliasTransaction, Transaction, TransactionType}
import monix.eval.Task
import monix.reactive.Observable
import org.iq80.leveldb.DB

import scala.jdk.CollectionConverters.*

package object common {
  import AddressTransactions.*
  import BalanceDistribution.*

  def addressTransactions(
      db: DB,
      useLiquidDiff: UseLiquidDiff,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[TransactionMeta] = {
    val addressTxs = allAddressTransactions(db, useLiquidDiff, subject, sender, types, fromId)
    def metas      = addressTxs.map { case (dbMeta, transaction) => toMeta(useLiquidDiff, db, dbMeta, transaction) }
    Observable.fromIterator(Task(metas))
  }

  private def toMeta(
      useLiquidDiff: UseLiquidDiff,
      db: DB,
      dbMeta: TxMeta,
      transaction: Transaction
  ): TransactionMeta =
    useLiquidDiff { maybeDiff =>
      def loadISR(t: Transaction) =
        maybeDiff
          .flatMap(_._2.scriptResults.get(t.id()))
          .orElse(loadInvokeScriptResult(db, t.id()))

      def loadETM(t: Transaction) =
        maybeDiff
          .flatMap(_._2.ethereumTransactionMeta.get(t.id()))
          .orElse(loadEthereumMetadata(db, t.id()))

      TransactionMeta.create(
        dbMeta.height,
        transaction,
        dbMeta.succeeded,
        dbMeta.spentComplexity,
        loadISR,
        loadETM
      )
    }

  def balanceDistribution(
      db: DB,
      height: Int,
      after: Option[Address],
      overrides: Map[Address, Portfolio],
      globalPrefix: Array[Byte],
      addressId: Array[Byte] => AddressId,
      balanceOf: Portfolio => Long
  ): Observable[(Address, Long)] =
    db.resourceObservable
      .flatMap { resource =>
        resource.iterator.seek(
          globalPrefix ++ after
            .flatMap(address => resource.get(Keys.addressId(address)))
            .fold(Array.emptyByteArray)(id => Longs.toByteArray(id.toLong + 1))
        )
        Observable.fromIterator(Task(new BalanceIterator(resource, globalPrefix, addressId, balanceOf, height, overrides).asScala.filter(_._2 > 0)))
      }

  def aliasesOfAddress(db: DB, useLiquidDiff: UseLiquidDiff, address: Address): Observable[(Height, CreateAliasTransaction)] = {
    val disabledAliases = db.get(Keys.disabledAliases)
    addressTransactions(db, useLiquidDiff, address, Some(address), Set(TransactionType.CreateAlias), None)
      .collect {
        case TransactionMeta(height, cat: CreateAliasTransaction, true) if disabledAliases.isEmpty || !disabledAliases(cat.alias) => height -> cat
      }
  }

  def loadTransactionMeta(db: DB, maybeDiff: => Option[(Int, Diff)])(tuple: (TxMeta, Transaction)): TransactionMeta = {
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
