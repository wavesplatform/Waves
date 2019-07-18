package com.wavesplatform.state.extensions.composite

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.extensions.AddressTransactions
import com.wavesplatform.state.{Diff, Height}
import com.wavesplatform.transaction.{Transaction, TransactionParser}
import com.wavesplatform.utils.CloseableIterator

private[state] final class CompositeAddressTransactions(baseProvider: AddressTransactions, height: Height, getDiff: () => Option[Diff]) extends AddressTransactions {
  override def addressTransactionsIterator(address: Address,
                                           types: Set[TransactionParser],
                                           fromId: Option[ByteStr]): CloseableIterator[(Height, Transaction)] = {
    val fromDiff = for {
      diff <- getDiff().iterator
      (tx, addresses) <- diff.transactions.valuesIterator
    } yield (height, tx, addresses)

    com.wavesplatform.state.addressTransactionsCompose(baseProvider, CloseableIterator.fromIterator(fromDiff))(address, types, fromId)
  }
}
