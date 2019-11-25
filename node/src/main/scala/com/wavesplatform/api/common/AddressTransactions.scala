package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, DBResource, Keys}
import com.wavesplatform.state.{AddressId, Diff, Height, InvokeScriptResult, TransactionId, TxNum}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Authorized, Transaction}
import monix.reactive.Observable
import org.iq80.leveldb.DB

trait AddressTransactions {
  import AddressTransactions._

  def addressTransactions(
      db: DB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(Height, Transaction)] =
    db.resourceObservable.flatMap { resource =>
      Observable.fromIterable(allAddressTransactions(resource, maybeDiff, subject, sender, types, fromId))
    }

  def invokeScriptResults(
      db: DB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(Height, Either[Transaction, (InvokeScriptTransaction, Option[InvokeScriptResult])])] =
    db.resourceObservable.flatMap { resource =>
      Observable
        .fromIterable(allAddressTransactions(resource, maybeDiff, subject, sender, types, fromId).map {
          case (h, ist: InvokeScriptTransaction) =>
            h -> Right(ist -> maybeDiff.flatMap(_._2.scriptResults.get(ist.id())).orElse(loadInvokeScriptResult(resource, ist.id())))
          case (h, tx) => h -> Left(tx)
        })
    }
}

object AddressTransactions {
  private def loadTransaction(resource: DBResource, height: Height, txNum: TxNum, sender: Option[Address]): Option[(Height, Transaction)] =
    resource.get(Keys.transactionAt(height, txNum)) match {
      case Some(tx: Authorized) if sender.forall(_ == tx.sender.toAddress) => Some(height -> tx)
      case _                                                               => None
    }

  private def loadInvokeScriptResult(resource: DBResource, txId: ByteStr): Option[InvokeScriptResult] =
    for {
      (h, txNum) <- resource.get(Keys.transactionHNById(TransactionId(txId)))
    } yield resource.get(Keys.invokeScriptResult(h, txNum))

  def allAddressTransactions(
      resource: DBResource,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterable[(Height, Transaction)] =
    transactionsFromDiff(maybeDiff, subject, sender, types, fromId) ++
      transactionsFromDB(
        resource,
        subject,
        sender,
        types,
        fromId.filter(id => maybeDiff.exists { case (_, diff) => !diff.transactionMap().contains(id) })
      )

  def transactionsFromDB(
      resource: DBResource,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterable[(Height, Transaction)] = resource.get(Keys.addressId(subject)).fold(Iterable.empty[(Height, Transaction)]) { intId =>
    val addressId = AddressId(intId)
    val (maxHeight, maxTxNum) =
      fromId.flatMap(id => resource.get(Keys.transactionHNById(TransactionId(id)))).getOrElse(Height(Int.MaxValue) -> TxNum(Short.MaxValue))

    (for {
      seqNr                    <- (resource.get(Keys.addressTransactionSeqNr(addressId)) to 1 by -1).view
      (height, transactionIds) <- resource.get(Keys.addressTransactionHN(addressId, seqNr)).view
      if height <= maxHeight
      (txType, txNum) <- transactionIds.reverse.view
    } yield (height, txNum, txType))
      .dropWhile { case (h, txNum, _) => h > maxHeight || h == maxHeight && txNum >= maxTxNum }
      .collect { case (h, txNum, txType) if (types.isEmpty || types(txType)) => h -> txNum }
      .flatMap { case (h, txNum) => loadTransaction(resource, h, txNum, sender) }

  }

  def transactionsFromDiff(
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterable[(Height, Transaction)] =
    (for {
      (h, diff)       <- maybeDiff.toSeq
      (tx, addresses) <- diff.transactions.reverse
      if addresses(subject)
    } yield h -> tx)
      .dropWhile { case (_, tx) => fromId.isDefined && !fromId.contains(tx.id()) }
      .filter { case (_, tx) => types.isEmpty || types.contains(tx.typeId) }
      .collect { case v @ (_, tx: Authorized) if sender.forall(_ == tx.sender.toAddress) => v }
}
