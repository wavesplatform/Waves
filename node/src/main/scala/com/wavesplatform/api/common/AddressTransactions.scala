package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{protobuf => pb}
import com.wavesplatform.database.{DBExt, DBResource, Keys}
import com.wavesplatform.state.{Diff, Height, InvokeScriptResult, NewTransactionInfo, TransactionId, TxNum}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Authorized, GenesisTransaction, Transaction}
import monix.eval.Task
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
  ): Observable[(Height, Transaction, Boolean)] =
    Observable.fromIterator(Task(allAddressTransactions(db, maybeDiff, subject, sender, types, fromId)))

  def invokeScriptResults(
      db: DB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[TransactionMeta] =
    Observable
      .fromIterator(Task(allAddressTransactions(db, maybeDiff, subject, sender, types, fromId).map {
        case (h, ist: InvokeScriptTransaction, status) =>
          (h, Right(ist -> maybeDiff.flatMap(_._2.scriptResults.get(ist.id())).orElse(loadInvokeScriptResult(db, ist.id()))), status)
        case (h, tx, status) => (h, Left(tx), status)
      }))
}

object AddressTransactions {
  type TransactionMeta = (Height, Either[Transaction, (InvokeScriptTransaction, Option[InvokeScriptResult])], Boolean)

  private def loadTransaction(db: DB, height: Height, txNum: TxNum, sender: Option[Address]): Option[(Height, Transaction, Boolean)] =
    db.get(Keys.transactionAt(height, txNum)) match {
      case Some((tx: Authorized, status)) if sender.forall(_ == tx.sender.toAddress) => Some((height, tx, status))
      case Some((gt: GenesisTransaction, status)) if sender.isEmpty                  => Some((height, gt, status))
      case _                                                                         => None
    }

  private def loadInvokeScriptResult(resource: DBResource, txId: ByteStr): Option[InvokeScriptResult] =
    for {
      pb.TransactionMeta(h, txNum, InvokeScriptTransaction.typeId, _) <- resource.get(Keys.transactionMetaById(TransactionId(txId)))
      r                                                               <- resource.get(Keys.invokeScriptResult(h, TxNum(txNum.toShort)))
    } yield r

  def loadInvokeScriptResult(db: DB, txId: ByteStr): Option[InvokeScriptResult] =
    db.withResource(r => loadInvokeScriptResult(r, txId))

  def allAddressTransactions(
      db: DB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterator[(Height, Transaction, Boolean)] =
    transactionsFromDiff(maybeDiff, subject, sender, types, fromId) ++
      transactionsFromDB(
        db,
        subject,
        sender,
        types,
        fromId.filter(id => maybeDiff.exists { case (_, diff) => !diff.transactions.contains(id) })
      )

  def transactionsFromDB(
      db: DB,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterator[(Height, Transaction, Boolean)] =
    db.get(Keys.addressId(subject))
      .fold(Iterable.empty[(Height, Transaction, Boolean)]) { addressId =>
        val (maxHeight, maxTxNum) =
          fromId
            .flatMap(id => db.get(Keys.transactionMetaById(TransactionId(id))))
            .fold[(Height, TxNum)](Height(Int.MaxValue) -> TxNum(Short.MaxValue)) { tm =>
              Height(tm.height) -> TxNum(tm.num.toShort)
            }

        (for {
          seqNr                    <- (db.get(Keys.addressTransactionSeqNr(addressId)) to 0 by -1).view
          (height, transactionIds) <- db.get(Keys.addressTransactionHN(addressId, seqNr)).view if height <= maxHeight
          (txType, txNum)          <- transactionIds.view
        } yield (height, txNum, txType))
          .dropWhile { case (h, txNum, _) => h > maxHeight || h == maxHeight && txNum >= maxTxNum }
          .collect { case (h, txNum, txType) if types.isEmpty || types(txType) => h -> txNum }
          .flatMap { case (h, txNum) => loadTransaction(db, h, txNum, sender) }
      }
      .iterator

  def transactionsFromDiff(
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterator[(Height, Transaction, Boolean)] =
    (for {
      (h, diff)                                 <- maybeDiff.toSeq
      NewTransactionInfo(tx, addresses, status) <- diff.transactions.values.toSeq.reverse
      if addresses(subject)
    } yield (h, tx, status))
      .dropWhile { case (_, tx, _) => fromId.isDefined && !fromId.contains(tx.id()) }
      .dropWhile { case (_, tx, _) => fromId.contains(tx.id()) }
      .filter { case (_, tx, _) => types.isEmpty || types.contains(tx.typeId) }
      .collect { case v @ (_, tx: Authorized, _) if sender.forall(_ == tx.sender.toAddress) => v }
      .iterator
}
