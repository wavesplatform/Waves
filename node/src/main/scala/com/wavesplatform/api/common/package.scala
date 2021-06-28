package com.wavesplatform.api

import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.database.{DBExt, Keys}
import com.wavesplatform.state.{Diff, Height}
import com.wavesplatform.transaction.{CreateAliasTransaction, TransactionType}
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
}
