package com.wavesplatform.api.common

import com.google.common.collect.AbstractIterator
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.AddressTransactions.TxByAddressIterator.BatchSize
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.{AddressId, DBExt, DBResource, Key, Keys, RDB, readTransactionHNSeqAndType}
import com.wavesplatform.state.{Height, InvokeScriptResult, StateSnapshot, TransactionId, TxMeta, TxNum}
import com.wavesplatform.transaction.{Authorized, EthereumTransaction, GenesisTransaction, Transaction, TransactionType}
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.RocksDB

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

object AddressTransactions {
  private def loadTransactions(
      db: DBResource,
      keys: ArrayBuffer[Key[Option[(TxMeta, Transaction)]]],
      nums: ArrayBuffer[TxNum],
      sizes: ArrayBuffer[Int],
      sender: Option[Address]
  ): Seq[(TxMeta, Transaction, TxNum)] =
    db.multiGet(keys, sizes)
      .zip(nums)
      .flatMap {
        case (Some((m, tx: Authorized)), txNum) if sender.forall(_ == tx.sender.toAddress)         => Some((m, tx, txNum))
        case (Some((m, gt: GenesisTransaction)), txNum) if sender.isEmpty                          => Some((m, gt, txNum))
        case (Some((m, et: EthereumTransaction)), txNum) if sender.forall(_ == et.senderAddress()) => Some((m, et, txNum))
        case _                                                                                     => None
      }
      .toSeq

  private def loadInvokeScriptResult(
      resource: DBResource,
      txMetaHandle: RDB.TxMetaHandle,
      apiHandle: RDB.ApiHandle,
      txId: ByteStr
  ): Option[InvokeScriptResult] =
    for {
      tm           <- resource.get(Keys.transactionMetaById(TransactionId(txId), txMetaHandle))
      scriptResult <- resource.get(Keys.invokeScriptResult(Height(tm.height), TxNum(tm.num.toShort), apiHandle))
    } yield scriptResult

  def loadInvokeScriptResult(db: RocksDB, txMetaHandle: RDB.TxMetaHandle, apiHandle: RDB.ApiHandle, txId: ByteStr): Option[InvokeScriptResult] =
    db.withResource(r => loadInvokeScriptResult(r, txMetaHandle, apiHandle, txId))

  def loadInvokeScriptResult(db: RocksDB, apiHandle: RDB.ApiHandle, height: Height, txNum: TxNum): Option[InvokeScriptResult] =
    db.get(Keys.invokeScriptResult(height, txNum, apiHandle))

  def loadEthereumMetadata(db: RocksDB, txMetaHandle: RDB.TxMetaHandle, apiHandle: RDB.ApiHandle, txId: ByteStr): Option[EthereumTransactionMeta] =
    db.withResource { resource =>
      for {
        tm <- resource.get(Keys.transactionMetaById(TransactionId(txId), txMetaHandle))
        m  <- resource.get(Keys.ethereumTransactionMeta(Height(tm.height), TxNum(tm.num.toShort), apiHandle))
      } yield m
    }

  def loadEthereumMetadata(db: RocksDB, apiHandle: RDB.ApiHandle, height: Height, txNum: TxNum): Option[EthereumTransactionMeta] =
    db.get(Keys.ethereumTransactionMeta(height, txNum, apiHandle))

  def allAddressTransactions(
      rdb: RDB,
      maybeSnapshot: Option[(Height, StateSnapshot)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction, TxNum)] = {
    val diffTxs = transactionsFromSnapshot(maybeSnapshot, subject, sender, types, fromId)

    val dbTxs = transactionsFromDB(
      rdb,
      subject,
      sender,
      types,
      fromId.filter(id => maybeSnapshot.exists(s => !s._2.transactions.contains(id)))
    )

    // TODO: temporary
    Observable.fromIterable(diffTxs) ++ dbTxs.filterNot { case (_, dbTx, _) =>
      diffTxs.exists { case (_, diffTx, _) => diffTx.id() == dbTx.id() }
    }
  }

  def transactionsFromDB(
      rdb: RDB,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction, TxNum)] =
    rdb.db.resourceObservable(rdb.apiHandle.handle).flatMap { dbResource =>
      dbResource
        .get(Keys.addressId(subject))
        .fold(Observable.empty[(TxMeta, Transaction, TxNum)]) { addressId =>
          val (maxHeight, maxTxNum) =
            fromId
              .flatMap(id => rdb.db.get(Keys.transactionMetaById(TransactionId(id), rdb.txMetaHandle)))
              .fold[(Height, TxNum)](Height(Int.MaxValue) -> TxNum(Short.MaxValue)) { tm =>
                Height(tm.height) -> TxNum(tm.num.toShort)
              }

          Observable
            .fromIterator(
              Task(new TxByAddressIterator(dbResource, rdb.txHandle, rdb.apiHandle, addressId, maxHeight, maxTxNum, sender, types).asScala)
            )
            .concatMapIterable(identity)
        }
    }

  private def transactionsFromSnapshot(
      maybeSnapshot: Option[(Height, StateSnapshot)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Seq[(TxMeta, Transaction, TxNum)] =
    (for {
      (height, snapshot) <- maybeSnapshot.toSeq
      (nti, idx)         <- snapshot.transactions.values.toSeq.zipWithIndex.reverse
      if nti.affected(subject)
    } yield (TxMeta(height, nti.status, nti.spentComplexity), nti.transaction, idx))
      .dropWhile { case (_, tx, _) => fromId.isDefined && !fromId.contains(tx.id()) }
      .dropWhile { case (_, tx, _) => fromId.contains(tx.id()) }
      .filter { case (_, tx, _) => types.isEmpty || types.contains(tx.tpe) }
      .collect { case (m, tx: Authorized, idx) if sender.forall(_ == tx.sender.toAddress) => (m, tx, TxNum(idx.toShort)) }

  private class TxByAddressIterator(
      db: DBResource,
      txHandle: RDB.TxHandle,
      apiHandle: RDB.ApiHandle,
      addressId: AddressId,
      maxHeight: Height,
      maxTxNum: TxNum,
      sender: Option[Address],
      types: Set[Transaction.Type]
  ) extends AbstractIterator[Seq[(TxMeta, Transaction, TxNum)]] {
    private val seqNr = db.get(Keys.addressTransactionSeqNr(addressId, apiHandle))
    db.withSafePrefixIterator(_.seekForPrev(Keys.addressTransactionHN(addressId, seqNr, apiHandle).keyBytes))(())

    final override def computeNext(): Seq[(TxMeta, Transaction, TxNum)] = db.withSafePrefixIterator { dbIterator =>
      val keysBuffer  = new ArrayBuffer[Key[Option[(TxMeta, Transaction)]]]()
      val numsBuffer  = new ArrayBuffer[TxNum]()
      val sizesBuffer = new ArrayBuffer[Int]()
      while (dbIterator.isValid && keysBuffer.length < BatchSize) {
        val (height, txs) = readTransactionHNSeqAndType(dbIterator.value())
        dbIterator.prev()
        if (height > maxHeight) {
          ()
        } else if (height == maxHeight) {
          txs
            .dropWhile { case (_, txNum, _) => txNum >= maxTxNum }
            .foreach { case (tp, txNum, size) =>
              if (types.isEmpty || types(TransactionType.fromId(tp))) {
                keysBuffer.addOne(Keys.transactionAt(height, txNum, txHandle))
                numsBuffer.addOne(txNum)
                sizesBuffer.addOne(size)
              }
            }
        } else {
          txs.foreach { case (tp, txNum, size) =>
            if (types.isEmpty || types(TransactionType.fromId(tp))) {
              keysBuffer.addOne(Keys.transactionAt(height, txNum, txHandle))
              numsBuffer.addOne(txNum)
              sizesBuffer.addOne(size)
            }
          }
        }
      }
      if (keysBuffer.nonEmpty) {
        loadTransactions(db, keysBuffer, numsBuffer, sizesBuffer, sender)
      } else
        endOfData()
    }(endOfData())
  }

  object TxByAddressIterator {
    val BatchSize = 50
  }
}
