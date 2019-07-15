package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.AddressTransactionsProvider
import com.wavesplatform.transaction.{Transaction, TransactionParser}
import com.wavesplatform.utils.CloseableIterator

private[state] final class BlockchainUpdaterImplAddressTransactionsProvider(baseProvider: AddressTransactionsProvider, getDiff: () => Option[Diff])
    extends AddressTransactionsProvider {
  override def addressTransactionsIterator(address: Address,
                                           types: Set[TransactionParser],
                                           fromId: Option[ByteStr]): CloseableIterator[(Height, Transaction)] = {
    addressTransactionsFromDiff(baseProvider, getDiff())(address, types, fromId)
  }
}
