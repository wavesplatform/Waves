package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import scorex.account.{Account, Alias}
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{PaymentTransaction, Transaction}

case class BlockDiff(txsDiff: Diff,
                     heightDiff: Int,
                     effectiveBalanceSnapshots: Seq[EffectiveBalanceSnapshot]
                    )

object BlockDiff {

  implicit class BlockDiffExt(bd: BlockDiff) {
    lazy val maxPaymentTransactionTimestamp: Map[Account, Long] = bd.txsDiff.transactions.toList
      .collect({ case (_, (_, ptx: PaymentTransaction, _)) => ptx.sender.toAccount -> ptx.timestamp })
      .groupBy(_._1)
      .mapValues(_.map(_._2).max)

    lazy val effectiveLeaseTxUpdates: (Set[EqByteArray], Set[EqByteArray]) = {
      val canceledLeaseIds: Set[EqByteArray] = bd.txsDiff.transactions.values.map(_._2)
        .filter(_.isInstanceOf[LeaseCancelTransaction])
        .map(_.asInstanceOf[LeaseCancelTransaction])
        .map(_.leaseId)
        .map(EqByteArray)
        .toSet


      val newLeaseIds = bd.txsDiff.transactions.values.map(_._2)
        .filter(_.isInstanceOf[LeaseTransaction])
        .map(_.asInstanceOf[LeaseTransaction])
        .map(_.id)
        .map(EqByteArray)
        .toSet

      val effectiveNewCancels = canceledLeaseIds.diff(newLeaseIds)
      val effectiveNewLeases = newLeaseIds.diff(canceledLeaseIds)
      (effectiveNewLeases, effectiveNewCancels)
    }

    lazy val orderExchangeTxsMap: Map[EqByteArray, Set[Array[Byte]]] = {
      Monoid.combineAll(
        bd.txsDiff.transactions
          .collect { case (_, (_, etx: ExchangeTransaction, _)) => etx }
          .map(etx => Map(
            EqByteArray(etx.buyOrder.id) -> Set(etx.id),
            EqByteArray(etx.sellOrder.id) -> Set(etx.id)
          )))
    }
  }

}

case class Diff(transactions: Map[ByteArray, (Int, Transaction, Set[Account])],
                portfolios: Map[Account, Portfolio],
                issuedAssets: Map[ByteArray, AssetInfo],
                aliases: Map[Alias, Account])

object Diff {
  def apply(height: Int, tx: Transaction,
            portfolios: Map[Account, Portfolio],
            assetInfos: Map[ByteArray, AssetInfo] = Map.empty,
            aliases: Map[Alias, Account] = Map.empty
           ): Diff = Diff(
    transactions = Map(EqByteArray(tx.id) -> (height, tx, portfolios.keys.toSet)),
    portfolios = portfolios,
    issuedAssets = assetInfos,
    aliases = aliases)

  implicit class DiffExt(d: Diff) {
    def asBlockDiff: BlockDiff = BlockDiff(d, 0, Seq.empty)

    lazy val accountTransactionIds: Map[Account, List[ByteArray]] = {
      val map: List[(Account, List[(Int, Long, ByteArray)])] = d.transactions.toList
        .flatMap { case (id, (h, tx, accs)) => accs.map(acc => acc -> List((h, tx.timestamp, id))) }
      val groupedByAcc = map.foldLeft(Map.empty[Account, List[(Int, Long, ByteArray)]]) { case (m, (acc, list)) =>
        m.combine(Map(acc -> list))
      }
      groupedByAcc
        .mapValues(l => l.sortBy { case ((h, t, id)) => (-h, -t) }) // fresh head ([h=2, h=1, h=0])
        .mapValues(_.map(_._3))
    }

    lazy val paymentTransactionIdsByHashes: Map[ByteArray, ByteArray] = d.transactions
      .collect { case (_, (_, ptx: PaymentTransaction, _)) => EqByteArray(ptx.hash) -> EqByteArray(ptx.id) }
  }

}

case class EffectiveBalanceSnapshot(acc: Account, height: Int, prevEffectiveBalance: Long, effectiveBalance: Long)

case class Portfolio(balance: Long, leaseInfo: LeaseInfo, assets: Map[ByteArray, Long]) {
  lazy val effectiveBalance: Long = safeSum(balance, leaseInfo.leaseIn) - leaseInfo.leaseOut
}

case class LeaseInfo(leaseIn: Long, leaseOut: Long)

object LeaseInfo {
  val empty = LeaseInfo(0, 0)
}

case class AssetInfo(isReissuable: Boolean, volume: Long)
