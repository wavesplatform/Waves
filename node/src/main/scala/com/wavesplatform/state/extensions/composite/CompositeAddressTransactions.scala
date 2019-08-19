package com.wavesplatform.state.extensions.composite

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.extensions.AddressTransactions
import com.wavesplatform.state.{Diff, Height}
import com.wavesplatform.transaction.{Transaction, TransactionParser}
import monix.reactive.Observable

private[state] final class CompositeAddressTransactions(baseProvider: AddressTransactions, height: Height, getDiff: () => Option[Diff]) extends AddressTransactions {
  override def addressTransactionsObservable(address: Address,
                                             types: Set[TransactionParser],
                                             fromId: Option[ByteStr]): Observable[(Height, Transaction)] = {
    val fromDiff = for {
      diff <- getDiff().toIterable
      (height, tx, addresses) <- diff.transactions.values.toVector.reverse
    } yield (Height(height), tx, addresses)

    com.wavesplatform.state.addressTransactionsCompose(baseProvider, Observable.fromIterable(fromDiff))(address, types, fromId)
  }
}
