package com.wavesplatform.api.common

import com.google.common.collect.AbstractIterator
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.{AddressId, DBExt, DBResource, KeyTags, Keys, readTransactionHNSeqAndType}
import com.wavesplatform.state.{Diff, Height, InvokeScriptResult, TransactionId, TxMeta, TxNum}
import com.wavesplatform.transaction.{Authorized, EthereumTransaction, GenesisTransaction, Transaction, TransactionType}
import monix.eval.Task
import monix.reactive.Observable
import org.rocksdb.{ReadOptions, RocksDB}

import scala.jdk.CollectionConverters.*
import scala.annotation.tailrec

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
  ): Observable[(TxMeta, Transaction, Option[TxNum])] = {
    db.get(Keys.addressId(subject))
      .fold(Observable.empty[(TxMeta, Transaction, Option[TxNum])]) { addressId =>
        val (maxHeight, maxTxNum) =
          fromId
            .flatMap(id => db.get(Keys.transactionMetaById(TransactionId(id))))
            .fold[(Height, TxNum)](Height(Int.MaxValue) -> TxNum(Short.MaxValue)) { tm =>
              Height(tm.height) -> TxNum(tm.num.toShort)
            }

        Observable
          .fromIterator(
            Task(new TxByAddressIterator(db, addressId, maxHeight, maxTxNum, types).asScala)
          )
          .concatMapIterable(_.flatMap { case (h, txNum) =>
            loadTransaction(db, h, txNum, sender)
              .map { case (m, tx) => (m, tx, Some(txNum)) }
          })

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

  class TxByAddressIterator(db: RocksDB, addressId: AddressId, maxHeight: Int, maxTxNum: Int, types: Set[Transaction.Type])
      extends AbstractIterator[Seq[(Height, TxNum)]] {
    val dbIterator = db.newIterator(new ReadOptions().setTotalOrderSeek(false).setPrefixSameAsStart(true))
    val prefix     = KeyTags.AddressTransactionHeightTypeAndNums.prefixBytes ++ addressId.toByteArray
    val seqNr      = db.get(Keys.addressTransactionSeqNr(addressId))

    dbIterator.seekForPrev(Keys.addressTransactionHN(addressId, seqNr).keyBytes)

    @tailrec
    final override def computeNext(): Seq[(Height, TxNum)] = {
      if (dbIterator.isValid) {
        val (height, txs) = readTransactionHNSeqAndType(dbIterator.value())
        dbIterator.prev()
        if (height > maxHeight) {
          computeNext()
        } else if (height == maxHeight) {
          txs.view
            .dropWhile { case (_, txNum) => txNum >= maxTxNum }
            .collect { case (tp, txNum) if types.isEmpty || types(TransactionType(tp)) => (height, txNum) }
            .toSeq
        } else {
          txs.collect { case (tp, txNum) if types.isEmpty || types(TransactionType(tp)) => (height, txNum) }
        }
      } else endOfData()
    }
  }
}
