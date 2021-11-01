package com.wavesplatform.api

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.AddressTransactions._
import com.wavesplatform.database.{DBExt, Keys}
import com.wavesplatform.state.{Diff, Height, TxMeta}
import com.wavesplatform.transaction.{CreateAliasTransaction, Transaction, TransactionType}
import monix.reactive.Observable
import org.iq80.leveldb.DB

package object common extends BalanceDistribution with AddressTransactions {
  def aliasesOfAddress(db: DB, maybeDiff: => Option[(Height, Diff)], address: Address): Observable[(Height, CreateAliasTransaction)] = {
    val disabledAliases = db.get(Keys.disabledAliases)
    addressTransactions(db, maybeDiff, address, Some(address), Set(TransactionType.CreateAlias), None)
      .collect {
        case TransactionMeta(height, cat: CreateAliasTransaction, true) if disabledAliases.isEmpty || !disabledAliases(cat.alias) => height -> cat
      }
  }

  def loadTransactionMeta(db: DB, maybeDiff: => Option[(Int, Diff)])(m: (TxMeta, Transaction)): TransactionMeta =
    TransactionMeta.create(
      m._1.height,
      m._2,
      m._1.succeeded,
      m._1.spentComplexity,
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
