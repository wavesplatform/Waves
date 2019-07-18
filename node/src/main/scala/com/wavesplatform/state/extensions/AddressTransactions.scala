package com.wavesplatform.state.extensions

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.{Transaction, TransactionParser, TransactionParsers}
import com.wavesplatform.utils.CloseableIterator
import monix.reactive.Observable

trait AddressTransactions {
  def addressTransactionsIterator(address: Address,
                                  types: Set[TransactionParser],
                                  fromId: Option[ByteStr]): CloseableIterator[(Height, Transaction)]
}

object AddressTransactions {
  def apply[T](value: T)(implicit ev: T => AddressTransactions): AddressTransactions = value

  trait Prov[T] {
    def addressTransactions(value: T): AddressTransactions
  }

  case object Empty extends AddressTransactions {
    override def addressTransactionsIterator(address: Address, types: Set[TransactionParser], fromId: Option[BlockId]): CloseableIterator[(Height, Transaction)] =
      CloseableIterator.empty
  }

  def createListEither(b: Blockchain, at: AddressTransactions)(address: Address, types: Set[Transaction.Type], count: Int, fromId: Option[BlockId]): Either[String, Seq[(Height, Transaction)]] = {
    def createTransactionsList(): Seq[(Height, Transaction)] =
      at.addressTransactionsIterator(address, TransactionParsers.forTypeSet(types), fromId)
        .take(count)
        .closeAfter(_.toVector)

    fromId match {
      case Some(id) => b.transactionInfo(id).toRight(s"Transaction $id does not exist").map(_ => createTransactionsList())
      case None => Right(createTransactionsList())
    }
  }

  implicit class AddressTransactionsProviderExt(p: AddressTransactions) {
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
