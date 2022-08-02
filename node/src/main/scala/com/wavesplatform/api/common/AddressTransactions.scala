package com.wavesplatform.api.common

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.{DBExt, DBResource, Keys}
import com.wavesplatform.state.{Diff, Height, InvokeScriptResult, TransactionId, TxMeta, TxNum}
import com.wavesplatform.transaction.{Authorized, EthereumTransaction, GenesisTransaction, Transaction, TransactionType}
import org.iq80.leveldb.DB

object AddressTransactions {
  private def loadTransaction(db: DB, height: Height, txNum: TxNum, sender: Option[Address]): Option[(TxMeta, Transaction)] =
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

  def loadInvokeScriptResult(db: DB, txId: ByteStr): Option[InvokeScriptResult] =
    db.withResource(r => loadInvokeScriptResult(r, txId))

  def loadEthereumMetadata(db: DB, txId: ByteStr): Option[EthereumTransactionMeta] = db.withResource { resource =>
    for {
      tm <- resource.get(Keys.transactionMetaById(TransactionId(txId)))
      m  <- resource.get(Keys.ethereumTransactionMeta(Height(tm.height), TxNum(tm.num.toShort)))
    } yield m
  }

  def allAddressTransactions(
      db: DB,
      useLiquidDiff: UseLiquidDiff,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterator[(TxMeta, Transaction)] =
    useLiquidDiff(maybeDiff =>
      transactionsFromDiff(maybeDiff, subject, sender, types, fromId) ++
        transactionsFromDB(
          db,
          subject,
          sender,
          types,
          fromId.filter(id => maybeDiff.exists { case (_, diff) => !diff.transactions.contains(id) })
        )
    )

  private def transactionsFromDB(
      db: DB,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterator[(TxMeta, Transaction)] =
    db.get(Keys.addressId(subject))
      .fold(Iterable.empty[(TxMeta, Transaction)]) { addressId =>
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
          .collect { case (h, txNum, txType) if types.isEmpty || types(TransactionType(txType)) => h -> txNum }
          .flatMap { case (h, txNum) => loadTransaction(db, h, txNum, sender) }
      }
      .iterator

  private def transactionsFromDiff(
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      fromId: Option[ByteStr]
  ): Iterator[(TxMeta, Transaction)] =
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
