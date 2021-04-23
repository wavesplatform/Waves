package com.wavesplatform.api
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi.TransactionMeta
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, Keys}
import com.wavesplatform.state.{Diff, Height}
import com.wavesplatform.transaction.CreateAliasTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import monix.reactive.Observable
import org.iq80.leveldb.DB

package object common extends BalanceDistribution with AddressTransactions {
  def aliasesOfAddress(db: DB, maybeDiff: => Option[(Height, Diff)], address: Address): Observable[(Height, CreateAliasTransaction)] = {
    val disabledAliases = db.get(Keys.disabledAliases)
    addressTransactions(db, maybeDiff, address, Some(address), Set(CreateAliasTransaction.typeId), None)
      .collect {
        case TransactionMeta(height, cat: CreateAliasTransaction, true) if disabledAliases.isEmpty || !disabledAliases(cat.alias) => height -> cat
      }
  }

  def activeLeases(
      db: DB,
      maybeDiff: Option[(Height, Diff)],
      address: Address,
      leaseIsActive: ByteStr => Boolean
  ): Observable[(Height, LeaseTransaction)] =
    addressTransactions(db, maybeDiff, address, None, Set(LeaseTransaction.typeId), None)
      .collect { case TransactionMeta(h, lt: LeaseTransaction, true) if leaseIsActive(lt.id()) => h -> lt }
}
