package com.wavesplatform.state

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.extensions.impl.CompositeApiExtensions
import com.wavesplatform.transaction.{Transaction, TransactionParsers}

import scala.concurrent.duration.Duration

package object utils {

  implicit class BlockchainAddressTransactionsList(b: Blockchain) {
    def addressTransactions(address: Address,
                            types: Set[Transaction.Type],
                            count: Int,
                            fromId: Option[ByteStr]): Either[String, Seq[(Height, Transaction)]] = {

      def createTransactionsList(): Seq[(Height, Transaction)] =
        CompositeApiExtensions(b).addressTransactionsObservable(address, TransactionParsers.forTypeSet(types), fromId)
          .take(count)
          .toListL
          .runSyncUnsafe(Duration.Inf)

      fromId match {
        case Some(id) => b.transactionInfo(id).toRight(s"Transaction $id does not exist").map(_ => createTransactionsList())
        case None => Right(createTransactionsList())
      }
    }
  }

}
