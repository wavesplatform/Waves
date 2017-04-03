package com.wavesplatform.state2

import java.util

import cats._
import cats.implicits._
import cats.Monoid
import com.wavesplatform.state2.reader.StateReaderImpl
import scorex.transaction.assets.exchange.ExchangeTransaction

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit
}

class StateWriterImpl(p: JavaMapStorage) extends StateReaderImpl(p) with StateWriter {

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = {
    val txsDiff = blockDiff.txsDiff

    txsDiff.transactions.foreach { case (id, (h, tx, _)) =>
      p.transactions.put(id.arr, (h, tx.bytes))
    }

    val newOrderEtxs: Map[EqByteArray, Set[Array[Byte]]] = Monoid.combineAll(
      txsDiff.transactions
        .collect { case (_, (_, etx: ExchangeTransaction, _)) => etx }
        .map(etx => Map(
          EqByteArray(etx.buyOrder.id) -> Set(etx.id),
          EqByteArray(etx.sellOrder.id) -> Set(etx.id)
        )))

    newOrderEtxs.foreach { case (oid, txIds) =>
      Option(p.exchangeTransactionsByOrder.get(oid.arr)) match {
        case Some(ll) =>
          txIds.foreach(txId => ll.add(txId))
          p.exchangeTransactionsByOrder.put(oid.arr, ll)
        case None =>
          val newList = new util.ArrayList[Array[Byte]]()
          txIds.foreach(txId => newList.add(txId))
          p.accountTransactionIds.put(oid.arr, newList)
      }
    }

    txsDiff.portfolios.foreach { case (account, portfolioDiff) =>
      val updatedPortfolio = accountPortfolio(account).combine(portfolioDiff)
      p.portfolios.put(account.bytes,
        (updatedPortfolio.balance,
          updatedPortfolio.effectiveBalance,
          updatedPortfolio.assets.map { case (k, v) => k.arr -> v }))
    }

    txsDiff.issuedAssets.foreach { case (id, assetInfo) =>
      val updated = (Option(p.assets.get(id.arr)) match {
        case None => Monoid[AssetInfo].empty
        case Some(existing) => AssetInfo(existing._1, existing._2)
      }).combine(assetInfo)

      p.assets.put(id.arr, (updated.isReissuable, updated.volume))
    }

    blockDiff.txsDiff.accountTransactionIds.foreach { case (acc, txIds) =>
      Option(p.accountTransactionIds.get(acc.bytes)) match {
        case Some(ll) =>
          txIds.reverse.foreach(txId => ll.add(0, txId.arr))
          p.accountTransactionIds.put(acc.bytes, ll)
        case None =>
          val newList = new util.ArrayList[Array[Byte]]()
          txIds.reverse.foreach(txId => newList.add(0, txId.arr))
          p.accountTransactionIds.put(acc.bytes, newList)
      }
    }

    blockDiff.txsDiff.paymentTransactionIdsByHashes.foreach { case (EqByteArray(hash), EqByteArray(id)) =>
      p.paymentTransactionHashes.put(hash, id)
    }

    blockDiff.effectiveBalanceSnapshots.foreach { ebs =>
      p.effectiveBalanceSnapshots.put((ebs.acc.bytes, ebs.height), (ebs.prevEffectiveBalance, ebs.effectiveBalance))
    }

    blockDiff.maxPaymentTransactionTimestamp.foreach { case (acc, ts) =>
      val old = maxPaymentTransactionTimestampInPreviousBlocks(acc)
      if (ts > old.getOrElse(0L))
        p.maxPaymentTransactionTimestampInPreviousBlocks.put(acc.bytes, ts)
    }

    p.setHeight(p.getHeight + blockDiff.heightDiff)

    p.commit()
  }
}


