package com.wavesplatform.api.common

import com.google.common.collect.AbstractIterator
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.AddressTransactions.TxByAddressIterator.BatchSize
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.{AddressId, DBExt, DBResource, Key, KeyTags, Keys, RDB, readTransactionHNSeqAndType}
import com.wavesplatform.state.{Diff, Height, InvokeScriptResult, TransactionId, TxMeta, TxNum}
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
  ): Seq[(TxMeta, Transaction, Option[TxNum])] =
    db.multiGet(keys, sizes)
      .zip(nums)
      .flatMap {
        case (Some((m, tx: Authorized)), txNum) if sender.forall(_ == tx.sender.toAddress)         => Some((m, tx, Some(txNum)))
        case (Some((m, gt: GenesisTransaction)), txNum) if sender.isEmpty                          => Some((m, gt, Some(txNum)))
        case (Some((m, et: EthereumTransaction)), txNum) if sender.forall(_ == et.senderAddress()) => Some((m, et, Some(txNum)))
        case _                                                                                     => None
      }
      .toSeq

  private def loadInvokeScriptResult(resource: DBResource, txMetaHandle: RDB.TxMetaHandle, txId: ByteStr): Option[InvokeScriptResult] =
    for {
      tm           <- resource.get(Keys.transactionMetaById(TransactionId(txId), txMetaHandle))
      scriptResult <- resource.get(Keys.invokeScriptResult(tm.height, TxNum(tm.num.toShort)))
    } yield scriptResult

  def loadInvokeScriptResult(db: RocksDB, txMetaHandle: RDB.TxMetaHandle, txId: ByteStr): Option[InvokeScriptResult] =
    db.withResource(r => loadInvokeScriptResult(r, txMetaHandle, txId))

  def loadInvokeScriptResult(db: RocksDB, height: Height, txNum: TxNum): Option[InvokeScriptResult] =
    db.get(Keys.invokeScriptResult(height, txNum))

  def loadEthereumMetadata(db: RocksDB, txMetaHandle: RDB.TxMetaHandle, txId: ByteStr): Option[EthereumTransactionMeta] = db.withResource {
    resource =>
      for {
        tm <- resource.get(Keys.transactionMetaById(TransactionId(txId), txMetaHandle))
        m  <- resource.get(Keys.ethereumTransactionMeta(Height(tm.height), TxNum(tm.num.toShort)))
      } yield m
  }

  def loadEthereumMetadata(db: RocksDB, height: Height, txNum: TxNum): Option[EthereumTransactionMeta] =
    db.get(Keys.ethereumTransactionMeta(height, txNum))

  def allAddressTransactions(
      rdb: RDB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction, Option[TxNum])] = {
    val diffTxs = transactionsFromDiff(maybeDiff, subject, sender, types, fromId)

    val dbTxs = transactionsFromDB(
      rdb,
      subject,
      sender,
      types,
      fromId.filter(id => maybeDiff.exists { case (_, diff) => !diff.containsTransaction(id) })
    )
    Observable.fromIterable(diffTxs) ++ dbTxs.filterNot(diffTxs.contains)
  }

  def transactionsFromDB(
      rdb: RDB,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction, Option[TxNum])] = rdb.db.resourceObservable.flatMap { dbResource =>
    dbResource
      .get(Keys.addressId(subject))
      .fold(Observable.empty[(TxMeta, Transaction, Option[TxNum])]) { addressId =>
        val (maxHeight, maxTxNum) =
          fromId
            .flatMap(id => rdb.db.get(Keys.transactionMetaById(TransactionId(id), rdb.txMetaHandle)))
            .fold[(Height, TxNum)](Height(Int.MaxValue) -> TxNum(Short.MaxValue)) { tm =>
              Height(tm.height) -> TxNum(tm.num.toShort)
            }

        Observable
          .fromIterator(
            Task(new TxByAddressIterator(dbResource, rdb.txHandle, addressId, maxHeight, maxTxNum, sender, types).asScala)
          )
          .concatMapIterable(identity)
      }
  }

  private def transactionsFromDiff(
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Seq[(TxMeta, Transaction, Option[TxNum])] =
    (for {
      (height, diff) <- maybeDiff.toSeq
      nti            <- diff.transactions.toSeq.reverse
      if nti.affected(subject)
    } yield (TxMeta(height, nti.applied, nti.spentComplexity), nti.transaction))
      .dropWhile { case (_, tx) => fromId.isDefined && !fromId.contains(tx.id()) }
      .dropWhile { case (_, tx) => fromId.contains(tx.id()) }
      .filter { case (_, tx) => types.isEmpty || types.contains(tx.tpe) }
      .collect { case (m, tx: Authorized) if sender.forall(_ == tx.sender.toAddress) => (m, tx, None) }

  class TxByAddressIterator(
      db: DBResource,
      txHandle: RDB.TxHandle,
      addressId: AddressId,
      maxHeight: Int,
      maxTxNum: Int,
      sender: Option[Address],
      types: Set[Transaction.Type]
  ) extends AbstractIterator[Seq[(TxMeta, Transaction, Option[TxNum])]] {
    val prefix: Array[Byte] = KeyTags.AddressTransactionHeightTypeAndNums.prefixBytes ++ addressId.toByteArray
    val seqNr: Int          = db.get(Keys.addressTransactionSeqNr(addressId))

    db.withSafePrefixIterator(_.seekForPrev(Keys.addressTransactionHN(addressId, seqNr).keyBytes))()

    final override def computeNext(): Seq[(TxMeta, Transaction, Option[TxNum])] = db.withSafePrefixIterator { dbIterator =>
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
              if (types.isEmpty || types(TransactionType(tp))) {
                keysBuffer.addOne(Keys.transactionAt(height, txNum, txHandle))
                numsBuffer.addOne(txNum)
                sizesBuffer.addOne(size)
              }
            }
        } else {
          txs.foreach { case (tp, txNum, size) =>
            if (types.isEmpty || types(TransactionType(tp))) {
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
