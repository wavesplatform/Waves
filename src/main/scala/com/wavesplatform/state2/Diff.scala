package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import scorex.account.{Account, Alias}
import scorex.transaction.Transaction
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

case class Snapshot(prevHeight: Int, balance: Long, effectiveBalance: Long)

object Snapshot {
  implicit val snapshotMonoid = new Monoid[Snapshot] {
    override def empty: Snapshot = ???

    override def combine(x: Snapshot, y: Snapshot): Snapshot = ???
  }
}

case class EffectiveBalanceSnapshot(acc: Account, height: Int, prevEffectiveBalance: Long, effectiveBalance: Long, prevBalance: Long, balance: Long)

case class LeaseInfo(leaseIn: Long, leaseOut: Long)

object LeaseInfo {
  val empty = LeaseInfo(0, 0)

  implicit val leaseInfoMonoid = new Monoid[LeaseInfo] {
    override def empty: LeaseInfo = LeaseInfo.empty

    override def combine(x: LeaseInfo, y: LeaseInfo): LeaseInfo = LeaseInfo(safeSum(x.leaseIn, y.leaseIn), safeSum(x.leaseOut, y.leaseOut))
  }
}

case class AssetInfo(isReissuable: Boolean, volume: Long)

object AssetInfo {
  implicit val assetInfoMonoid = new Monoid[AssetInfo] {
    override def empty: AssetInfo = AssetInfo(isReissuable = true, 0)

    override def combine(x: AssetInfo, y: AssetInfo): AssetInfo
    = AssetInfo(x.isReissuable && y.isReissuable, x.volume + y.volume)
  }
}

case class Diff(transactions: Map[ByteArray, (Int, Transaction, Set[Account])],
                portfolios: Map[Account, Portfolio],
                issuedAssets: Map[ByteArray, AssetInfo],
                aliases: Map[Alias, Account],
                paymentTransactionIdsByHashes: Map[ByteArray, ByteArray],
                patchExtraLeaseIdsToCancel: Seq[ByteArray])

object Diff {

  def apply(portfolios: Map[Account, Portfolio]): Diff = new Diff(Map.empty, portfolios, Map.empty, Map.empty, Map.empty, Seq.empty)

  def apply(height: Int, tx: Transaction,
            portfolios: Map[Account, Portfolio],
            assetInfos: Map[ByteArray, AssetInfo] = Map.empty,
            aliases: Map[Alias, Account] = Map.empty,
            paymentTransactionIdsByHashes: Map[ByteArray, ByteArray] = Map.empty
           ): Diff = Diff(
    transactions = Map(EqByteArray(tx.id) -> (height, tx, portfolios.keys.toSet)),
    portfolios = portfolios,
    issuedAssets = assetInfos,
    aliases = aliases,
    paymentTransactionIdsByHashes = paymentTransactionIdsByHashes,
    patchExtraLeaseIdsToCancel = Seq.empty)

  implicit class DiffExt(d: Diff) {
    def asBlockDiff: BlockDiff = BlockDiff(d, 0, Map.empty)

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

    lazy val effectiveLeaseTxUpdates: (Set[EqByteArray], Set[EqByteArray]) = {
      val txs = d.transactions.values.map(_._2)

      val canceledLeaseIds: Set[EqByteArray] = txs
        .collect { case (lctx: LeaseCancelTransaction) => EqByteArray(lctx.leaseId) }
        .toSet

      val newLeaseIds = txs
        .collect { case (ltx: LeaseTransaction) => EqByteArray(ltx.id) }
        .toSet

      val effectiveNewCancels = (canceledLeaseIds ++ d.patchExtraLeaseIdsToCancel).diff(newLeaseIds)
      val effectiveNewLeases = newLeaseIds.diff(canceledLeaseIds ++ d.patchExtraLeaseIdsToCancel)
      (effectiveNewLeases, effectiveNewCancels)
    }

    lazy val orderExchangeTxsMap: Map[EqByteArray, Set[Array[Byte]]] = {
      Monoid.combineAll(
        d.transactions
          .collect { case (_, (_, etx: ExchangeTransaction, _)) => etx }
          .map(etx => Map(
            EqByteArray(etx.buyOrder.id) -> Set(etx.id),
            EqByteArray(etx.sellOrder.id) -> Set(etx.id)
          )))
    }
  }

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff(transactions = Map.empty, portfolios = Map.empty, issuedAssets = Map.empty, Map.empty, Map.empty, Seq.empty)

    override def combine(older: Diff, newer: Diff): Diff = Diff(
      transactions = older.transactions ++ newer.transactions,
      portfolios = older.portfolios.combine(newer.portfolios),
      issuedAssets = older.issuedAssets.combine(newer.issuedAssets),
      aliases = older.aliases ++ newer.aliases,
      paymentTransactionIdsByHashes = older.paymentTransactionIdsByHashes ++ newer.paymentTransactionIdsByHashes,
      patchExtraLeaseIdsToCancel = newer.patchExtraLeaseIdsToCancel ++ older.patchExtraLeaseIdsToCancel
    )
  }

}
