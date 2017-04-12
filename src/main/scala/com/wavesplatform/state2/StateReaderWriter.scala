package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import com.wavesplatform.state2.reader.StateReaderImpl
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}


trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit
}

class StateWriterImpl(p: JavaMapStorage) extends StateReaderImpl(p) with StateWriter {

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = {
    val txsDiff = blockDiff.txsDiff

    txsDiff.transactions.foreach { case (id, (h, tx, _)) =>
      p.transactions.put(id.arr, (h, tx.bytes))
    }

    blockDiff.txsDiff.orderExchangeTxsMap.foreach { case (oid, txIds) =>
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
          (updatedPortfolio.leaseInfo.leaseIn, updatedPortfolio.leaseInfo.leaseOut),
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
          // [h=12, h=11, h=10] ++ [h=9, ...]
          p.accountTransactionIds.put(acc.bytes, txIds.map(_.arr) ++ ll)
        case None =>
          // [h=2, h=1, h=0]
          p.accountTransactionIds.put(acc.bytes, txIds.map(_.arr))
      }
    }

    blockDiff.txsDiff.paymentTransactionIdsByHashes.foreach { case (EqByteArray(hash), EqByteArray(id)) =>
      p.paymentTransactionHashes.put(hash, id)
    }

    blockDiff.effectiveBalanceSnapshots.foreach { ebs =>
      p.effectiveBalanceSnapshots.put((ebs.acc.bytes, ebs.height), (ebs.prevEffectiveBalance, ebs.effectiveBalance))
    }


    blockDiff.txsDiff.aliases.foreach { case (alias, acc) =>
      p.aliasToAddress.put(alias.name, acc.bytes)
    }

    val (effectiveNewLeases, effectiveNewCancels) = blockDiff.txsDiff.effectiveLeaseTxUpdates

    effectiveNewLeases.foreach(id => p.leaseState.put(id.arr, true))
    effectiveNewCancels.foreach(id => p.leaseState.put(id.arr, false))

    p.setHeight(p.getHeight + blockDiff.heightDiff)

    p.commit()
  }
}


