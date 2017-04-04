package com.wavesplatform.state2

import java.util

import cats.Monoid
import cats.implicits._
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
          p.exchangeTransactionsByOrder.put(oid.arr, ll ++ txIds)
        case None =>
          p.accountTransactionIds.put(oid.arr, txIds.toList)
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
          p.accountTransactionIds.put(acc.bytes, txIds.map(_.arr).reverse ++ll)
        case None =>
          p.accountTransactionIds.put(acc.bytes, txIds.map(_.arr).reverse)
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


