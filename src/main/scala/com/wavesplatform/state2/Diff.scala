package com.wavesplatform.state2

import cats.Monoid
import cats.implicits._
import scorex.account.{Address, Alias}
import scorex.transaction.Transaction
import scorex.transaction.smart.Script

case class Snapshot(prevHeight: Int, balance: Long, effectiveBalance: Long)

case class LeaseInfo(leaseIn: Long, leaseOut: Long)

object LeaseInfo {
  val empty = LeaseInfo(0, 0)
  implicit val leaseInfoMonoid = new Monoid[LeaseInfo] {
    override def empty: LeaseInfo = LeaseInfo.empty

    override def combine(x: LeaseInfo, y: LeaseInfo): LeaseInfo = LeaseInfo(safeSum(x.leaseIn, y.leaseIn), safeSum(x.leaseOut, y.leaseOut))
  }
}

case class OrderFillInfo(volume: Long, fee: Long)

object OrderFillInfo {
  implicit val orderFillInfoMonoid = new Monoid[OrderFillInfo] {
    override def empty: OrderFillInfo = OrderFillInfo(0, 0)

    override def combine(x: OrderFillInfo, y: OrderFillInfo): OrderFillInfo = OrderFillInfo(x.volume + y.volume, x.fee + y.fee)
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

case class AccountDataInfo(data: Map[String, DataEntry[_]])

object AccountDataInfo {
  implicit val accountDataInfoMonoid = new Monoid[AccountDataInfo] {
    override def empty: AccountDataInfo = AccountDataInfo(Map.empty)

    override def combine(x: AccountDataInfo, y: AccountDataInfo): AccountDataInfo = AccountDataInfo(x.data ++ y.data)
  }
}

case class Diff(transactions: Map[ByteStr, (Int, Transaction, Set[Address])],
                portfolios: Map[Address, Portfolio],
                issuedAssets: Map[ByteStr, AssetInfo],
                aliases: Map[Alias, Address],
                orderFills: Map[ByteStr, OrderFillInfo],
                leaseState: Map[ByteStr, Boolean],
                scripts: Map[Address, Option[Script]],
                accountData: Map[Address, AccountDataInfo]) {

  lazy val accountTransactionIds: Map[Address, List[ByteStr]] = {
    val map: List[(Address, Set[(Int, Long, ByteStr)])] = transactions.toList
      .flatMap { case (id, (h, tx, accs)) => accs.map(acc => acc -> Set((h, tx.timestamp, id))) }
    val groupedByAcc = map.foldLeft(Map.empty[Address, Set[(Int, Long, ByteStr)]]) { case (m, (acc, set)) =>
      m.combine(Map(acc -> set))
    }
    groupedByAcc
      .mapValues(l => l.toList.sortBy { case ((h, t, _)) => (-h, -t) }) // fresh head ([h=2, h=1, h=0])
      .mapValues(_.map(_._3))
  }
}

object Diff {
  def apply(height: Int, tx: Transaction,
            portfolios: Map[Address, Portfolio] = Map.empty,
            assetInfos: Map[ByteStr, AssetInfo] = Map.empty,
            aliases: Map[Alias, Address] = Map.empty,
            orderFills: Map[ByteStr, OrderFillInfo] = Map.empty,
            leaseState: Map[ByteStr, Boolean] = Map.empty,
            scripts: Map[Address, Option[Script]] = Map.empty,
            accountData: Map[Address, AccountDataInfo] = Map.empty): Diff = Diff(
    transactions = Map((tx.id(), (height, tx, portfolios.keys.toSet))),
    portfolios = portfolios,
    issuedAssets = assetInfos,
    aliases = aliases,
    orderFills = orderFills,
    leaseState = leaseState,
    scripts = scripts,
    accountData = accountData)

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
      orderFills = older.orderFills.combine(newer.orderFills),
      leaseState = older.leaseState ++ newer.leaseState,
      scripts = older.scripts ++ newer.scripts,
      accountData = older.accountData.combine(newer.accountData))
  }
}
