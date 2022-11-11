package com.wavesplatform.api.common

import com.google.common.collect.AbstractIterator
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.AddressTransactions.TxByAddressIterator.BatchSize
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.{AddressId, DBExt, DBResource, Key, KeyTags, Keys, readTransactionHNSeqAndType}
import com.wavesplatform.state.{Diff, Height, InvokeScriptResult, TransactionId, TxMeta, TxNum}
import com.wavesplatform.transaction.{Authorized, EthereumTransaction, GenesisTransaction, Transaction, TransactionType}
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.{ReadOptions, RocksDB, RocksIterator}

import scala.jdk.CollectionConverters.*
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object AddressTransactions {
  private def loadTransactions(
      db: DBResource,
      keys: ArrayBuffer[(Key[Option[(TxMeta, Transaction)]], TxNum)],
      sender: Option[Address]
  ): Seq[(TxMeta, Transaction, Option[TxNum])] =
    db.multiGet(keys)
      .flatMap {
        case (Some((m, tx: Authorized)), txNum) if sender.forall(_ == tx.sender.toAddress)         => Some((m, tx, Some(txNum)))
        case (Some((m, gt: GenesisTransaction)), txNum) if sender.isEmpty                          => Some((m, gt, Some(txNum)))
        case (Some((m, et: EthereumTransaction)), txNum) if sender.forall(_ == et.senderAddress()) => Some((m, et, Some(txNum)))
        case _                                                                                     => None
      }
      .toSeq

  private def loadInvokeScriptResult(resource: DBResource, txId: ByteStr): Option[InvokeScriptResult] =
    for {
      tm           <- resource.get(Keys.transactionMetaById(TransactionId(txId)))
      scriptResult <- resource.get(Keys.invokeScriptResult(tm.height, TxNum(tm.num.toShort)))
    } yield scriptResult

  def loadInvokeScriptResult(db: RocksDB, txId: ByteStr): Option[InvokeScriptResult] =
    db.withResource(r => loadInvokeScriptResult(r, txId))

  def loadInvokeScriptResult(db: RocksDB, height: Height, txNum: TxNum): Option[InvokeScriptResult] =
    db.get(Keys.invokeScriptResult(height, txNum))

  def loadEthereumMetadata(db: RocksDB, txId: ByteStr): Option[EthereumTransactionMeta] = db.withResource { resource =>
    for {
      tm <- resource.get(Keys.transactionMetaById(TransactionId(txId)))
      m  <- resource.get(Keys.ethereumTransactionMeta(Height(tm.height), TxNum(tm.num.toShort)))
    } yield m
  }

  def loadEthereumMetadata(db: RocksDB, height: Height, txNum: TxNum): Option[EthereumTransactionMeta] =
    db.get(Keys.ethereumTransactionMeta(height, txNum))

  def allAddressTransactions(
      db: RocksDB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction, Option[TxNum])] =
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
  ): Observable[(TxMeta, Transaction, Option[TxNum])] = db.resourceObservable.flatMap { dbResource =>
    dbResource
      .get(Keys.addressId(subject))
      .fold(Observable.empty[(TxMeta, Transaction, Option[TxNum])]) { addressId =>
        val (maxHeight, maxTxNum) =
          fromId
            .flatMap(id => db.get(Keys.transactionMetaById(TransactionId(id))))
            .fold[(Height, TxNum)](Height(Int.MaxValue) -> TxNum(Short.MaxValue)) { tm =>
              Height(tm.height) -> TxNum(tm.num.toShort)
            }

        Observable
          .fromIterator(
            Task(new TxByAddressIterator(dbResource, addressId, maxHeight, maxTxNum, sender, types).asScala)
          )
          .concatMapIterable(identity)
      }
  }

  def transactionsFromDiff(
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Observable[(TxMeta, Transaction, Option[TxNum])] = {
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
          .collect { case (m, tx: Authorized) if sender.forall(_ == tx.sender.toAddress) => (m, tx, None) }
          .iterator
      }
    )
  }

  class TxByAddressIterator(
      db: DBResource,
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
      val buffer = new ArrayBuffer[(Key[Option[(TxMeta, Transaction)]], TxNum)]()
      while (dbIterator.isValid && buffer.length < BatchSize) {
        val (height, txs) = readTransactionHNSeqAndType(dbIterator.value())
        dbIterator.prev()
        if (height > maxHeight) {
          ()
        } else if (height == maxHeight) {
          buffer ++= txs.view
            .dropWhile { case (_, txNum) => txNum >= maxTxNum }
            .collect { case (tp, txNum) if types.isEmpty || types(TransactionType(tp)) => Keys.transactionAt(height, txNum) -> txNum }
        } else {
          buffer ++= txs.collect { case (tp, txNum) if types.isEmpty || types(TransactionType(tp)) => Keys.transactionAt(height, txNum) -> txNum }
        }
      }
      if (buffer.nonEmpty) {
        loadTransactions(db, buffer, sender)
      } else
        endOfData()
    }(endOfData())
  }

  object TxByAddressIterator {
    val BatchSize = 50
  }
}
