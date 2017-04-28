package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import scorex.account.{Account, Alias}
import scorex.transaction.Transaction
import scorex.transaction.assets.exchange.ExchangeTransaction

case class Snapshot(prevHeight: Int, balance: Long, effectiveBalance: Long)

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
                leaseState: Map[ByteArray, Boolean],
                assetsWithUniqueNames: Map[ByteArray, ByteArray]) {

  lazy val accountTransactionIds: Map[Account, List[ByteArray]] = {
    val map: List[(Account, Set[(Int, Long, ByteArray)])] = transactions.toList
      .flatMap { case (id, (h, tx, accs)) => accs.map(acc => acc -> Set((h, tx.timestamp, id))) }
    val groupedByAcc = map.foldLeft(Map.empty[Account, Set[(Int, Long, ByteArray)]]) { case (m, (acc, set)) =>
      m.combine(Map(acc -> set))
    }
    groupedByAcc
      .mapValues(l => l.toList.sortBy { case ((h, t, id)) => (-h, -t) }) // fresh head ([h=2, h=1, h=0])
      .mapValues(_.map(_._3))
  }
}

object Diff {
  def apply(height: Int, tx: Transaction,
            portfolios: Map[Account, Portfolio] = Map.empty,
            assetInfos: Map[ByteArray, AssetInfo] = Map.empty,
            aliases: Map[Alias, Account] = Map.empty,
            previousExchangeTxs: Map[ByteArray, Set[ExchangeTransaction]] = Map.empty,
            paymentTransactionIdsByHashes: Map[ByteArray, ByteArray] = Map.empty,
            leaseState: Map[ByteArray, Boolean] = Map.empty,
            assetsWithUniqueNames: Map[ByteArray, ByteArray] = Map.empty
           ): Diff = Diff(
    transactions = Map(EqByteArray(tx.id) -> (height, tx, portfolios.keys.toSet)),
    portfolios = portfolios,
    issuedAssets = assetInfos,
    aliases = aliases,
    paymentTransactionIdsByHashes = paymentTransactionIdsByHashes,
    previousExchangeTxs = previousExchangeTxs,
    leaseState = leaseState,
    assetsWithUniqueNames = assetsWithUniqueNames)

  val empty = new Diff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  implicit class DiffExt(d: Diff) {
    def asBlockDiff: BlockDiff = BlockDiff(d, 0, Map.empty)
  }

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff.empty

    override def combine(older: Diff, newer: Diff): Diff = Diff(
      transactions = older.transactions ++ newer.transactions,
      portfolios = older.portfolios.combine(newer.portfolios),
      issuedAssets = older.issuedAssets.combine(newer.issuedAssets),
      aliases = older.aliases ++ newer.aliases,
      paymentTransactionIdsByHashes = older.paymentTransactionIdsByHashes ++ newer.paymentTransactionIdsByHashes,
      previousExchangeTxs = older.previousExchangeTxs ++ newer.previousExchangeTxs,
      leaseState = older.leaseState ++ newer.leaseState,
      assetsWithUniqueNames = older.assetsWithUniqueNames ++ newer.assetsWithUniqueNames)
  }
}
