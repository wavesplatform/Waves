package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import scorex.account.{Account, Alias}
import scorex.transaction.Transaction
import scorex.transaction.assets.exchange.ExchangeTransaction

case class Snapshot(prevHeight: Int, balance: Long, effectiveBalance: Long)

object Snapshot {
  implicit val snapshotMonoid = new Monoid[Snapshot] {
    override def empty: Snapshot = ???

    override def combine(x: Snapshot, y: Snapshot): Snapshot = ???
  }
}

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
                previousExchangeTxs: Map[ByteArray, Set[ExchangeTransaction]],
                leaseState: Map[ByteArray, Boolean]) {

  lazy val accountTransactionIds: Map[Account, List[ByteArray]] = {
    val map: List[(Account, List[(Int, Long, ByteArray)])] = transactions.toList
      .flatMap { case (id, (h, tx, accs)) => accs.map(acc => acc -> List((h, tx.timestamp, id))) }
    val groupedByAcc = map.foldLeft(Map.empty[Account, List[(Int, Long, ByteArray)]]) { case (m, (acc, list)) =>
      m.combine(Map(acc -> list))
    }
    groupedByAcc
      .mapValues(l => l.sortBy { case ((h, t, id)) => (-h, -t) }) // fresh head ([h=2, h=1, h=0])
      .mapValues(_.map(_._3))
  }
}

object Diff {
  def apply(height: Int, tx: Transaction,
            portfolios: Map[Account, Portfolio],
            assetInfos: Map[ByteArray, AssetInfo] = Map.empty,
            aliases: Map[Alias, Account] = Map.empty,
            previousExchangeTxs: Map[ByteArray, Set[ExchangeTransaction]] = Map.empty,
            paymentTransactionIdsByHashes: Map[ByteArray, ByteArray] = Map.empty,
            leaseState: Map[ByteArray, Boolean] = Map.empty
           ): Diff = Diff(
    transactions = Map(EqByteArray(tx.id) -> (height, tx, portfolios.keys.toSet)),
    portfolios = portfolios,
    issuedAssets = assetInfos,
    aliases = aliases,
    paymentTransactionIdsByHashes = paymentTransactionIdsByHashes,
    previousExchangeTxs = previousExchangeTxs,
    leaseState = leaseState)

  implicit class DiffExt(d: Diff) {
    def asBlockDiff: BlockDiff = BlockDiff(d, 0, Map.empty)
  }

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff(transactions = Map.empty, portfolios = Map.empty, issuedAssets = Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

    override def combine(older: Diff, newer: Diff): Diff = Diff(
      transactions = older.transactions ++ newer.transactions,
      portfolios = older.portfolios.combine(newer.portfolios),
      issuedAssets = older.issuedAssets.combine(newer.issuedAssets),
      aliases = older.aliases ++ newer.aliases,
      paymentTransactionIdsByHashes = older.paymentTransactionIdsByHashes ++ newer.paymentTransactionIdsByHashes,
      previousExchangeTxs = older.previousExchangeTxs ++ newer.previousExchangeTxs,
      leaseState = older.leaseState ++ newer.leaseState)
  }
}
