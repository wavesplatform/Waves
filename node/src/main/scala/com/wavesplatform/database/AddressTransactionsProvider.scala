package com.wavesplatform.database

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.{Transaction, TransactionParser}
import com.wavesplatform.utils.CloseableIterator
import monix.reactive.Observable

trait AddressTransactionsProvider {
  def addressTransactionsIterator(address: Address,
                                  types: Set[TransactionParser],
                                  fromId: Option[ByteStr]): CloseableIterator[(Height, Transaction)]
}

object AddressTransactionsProvider {
  implicit class AddressTransactionsProviderExt(p: AddressTransactionsProvider) {
    def addressTransactionsObs(address: Address, types: Set[TransactionParser], fromId: Option[ByteStr]): Observable[(Height, Transaction)] =
      Observable.defer {
        val iterator = p.addressTransactionsIterator(address, types, fromId)
        Observable.fromIterator(iterator, () => iterator.close())
      }

    def collectAddressTransactions[T](address: Address, types: Set[TransactionParser], fromId: Option[ByteStr], count: Int = Int.MaxValue)(
      pf: PartialFunction[(Height, Transaction), T]): Seq[T] = {
      p.addressTransactionsIterator(address, types, fromId)
        .collect { case heightAndTx if pf.isDefinedAt(heightAndTx) => pf(heightAndTx) }
        .closeAfter(_.toVector)
    }
  }
}
