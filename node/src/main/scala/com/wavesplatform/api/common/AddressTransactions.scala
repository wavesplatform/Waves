package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.{DBExt, DBResource, Keys}
import com.wavesplatform.state.{Diff, Height, InvokeScriptResult, TransactionId, TxMeta, TxNum}
import com.wavesplatform.transaction.{Authorized, EthereumTransaction, GenesisTransaction, Transaction, TransactionType}
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.RocksDB

object AddressTransactions {
  private def loadTransaction(db: RocksDB, height: Height, txNum: TxNum, sender: Option[Address]): Option[(TxMeta, Transaction)] =
    db.get(Keys.transactionAt(height, txNum)) match {
      case Some((m, tx: Authorized)) if sender.forall(_ == tx.sender.toAddress)         => Some(m -> tx)
      case Some((m, gt: GenesisTransaction)) if sender.isEmpty                          => Some(m -> gt)
      case Some((m, et: EthereumTransaction)) if sender.forall(_ == et.senderAddress()) => Some(m -> et)
      case _                                                                            => None
    }

  private def loadInvokeScriptResult(resource: DBResource, txId: ByteStr): Option[InvokeScriptResult] =
    for {
      tm           <- resource.get(Keys.transactionMetaById(TransactionId(txId)))
      scriptResult <- resource.get(Keys.invokeScriptResult(tm.height, TxNum(tm.num.toShort)))
    } yield scriptResult

  def loadInvokeScriptResult(db: RocksDB, txId: ByteStr): Option[InvokeScriptResult] =
    db.withResource(r => loadInvokeScriptResult(r, txId))

  def loadEthereumMetadata(db: RocksDB, txId: ByteStr): Option[EthereumTransactionMeta] = db.withResource { resource =>
    for {
      tm <- resource.get(Keys.transactionMetaById(TransactionId(txId)))
      m  <- resource.get(Keys.ethereumTransactionMeta(Height(tm.height), TxNum(tm.num.toShort)))
    } yield m
  }

  def allAddressTransactions(
      db: RocksDB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction)] =
    transactionsFromDiff(maybeDiff, subject, sender, types, fromId) ++
      transactionsFromDB(
        db,
        subject,
        sender,
        types,
        fromId.filter(id => maybeDiff.exists { case (_, diff) => !diff.transactions.contains(id) })
      )

  def transactionsFromDB(
      db: RocksDB,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction)] = {
    db.get(Keys.addressId(subject))
      .fold(Observable.empty[(TxMeta, Transaction)]) { addressId =>
        val (maxHeight, maxTxNum) =
          fromId
            .flatMap(id => db.get(Keys.transactionMetaById(TransactionId(id))))
            .fold[(Height, TxNum)](Height(Int.MaxValue) -> TxNum(Short.MaxValue)) { tm =>
              Height(tm.height) -> TxNum(tm.num.toShort)
            }

        Observable
          .fromIterator(
            Task {
              (for {
                seqNr                    <- (db.get(Keys.addressTransactionSeqNr(addressId)) to 0 by -1).view
                (height, transactionIds) <- db.get(Keys.addressTransactionHN(addressId, seqNr)).view if height <= maxHeight
                (txType, txNum)          <- transactionIds.view
              } yield (height, txNum, txType))
                .dropWhile { case (h, txNum, _) => h > maxHeight || h == maxHeight && txNum >= maxTxNum }
                .collect { case (h, txNum, txType) if types.isEmpty || types(TransactionType(txType)) => h -> txNum }
                .iterator
            }
          )
          .concatMapIterable { case (h, txNum) =>
            loadTransaction(db, h, txNum, sender).toSeq
          }
      }
  }

  def transactionsFromDiff(
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction)] = {
    Observable.fromIterator(
      Task {
        (for {
          (height, diff) <- maybeDiff.toSeq
          nti            <- diff.transactions.values.toSeq.reverse
          if nti.affected(subject)
        } yield (TxMeta(height, nti.applied, nti.spentComplexity), nti.transaction))
          .dropWhile { case (_, tx) => fromId.isDefined && !fromId.contains(tx.id()) }
          .dropWhile { case (_, tx) => fromId.contains(tx.id()) }
          .filter { case (_, tx) => types.isEmpty || types.contains(tx.tpe) }
          .collect { case v @ (_, tx: Authorized) if sender.forall(_ == tx.sender.toAddress) => v }
          .iterator
      }
    )
  }
}
