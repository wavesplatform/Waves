package com.wavesplatform.state

import cats.implicits._
import cats.kernel.Monoid
import scorex.account.{Address, Alias, PublicKeyAccount}
import scorex.transaction.smart.script.Script
import scorex.transaction.{AssetId, Transaction}

case class LeaseBalance(in: Long, out: Long)

object LeaseBalance {
  val empty = LeaseBalance(0, 0)

  implicit val m: Monoid[LeaseBalance] = new Monoid[LeaseBalance] {
    override def empty: LeaseBalance = LeaseBalance.empty

    override def combine(x: LeaseBalance, y: LeaseBalance): LeaseBalance =
      LeaseBalance(safeSum(x.in, y.in), safeSum(x.out, y.out))
  }
}

case class VolumeAndFee(volume: Long, fee: Long)

object VolumeAndFee {
  val empty = VolumeAndFee(0, 0)

  implicit val m: Monoid[VolumeAndFee] = new Monoid[VolumeAndFee] {
    override def empty: VolumeAndFee = VolumeAndFee.empty

    override def combine(x: VolumeAndFee, y: VolumeAndFee): VolumeAndFee =
      VolumeAndFee(x.volume + y.volume, x.fee + y.fee)
  }
}

case class AssetInfo(isReissuable: Boolean, volume: BigInt, script: Option[Script])
object AssetInfo {
  implicit val assetInfoMonoid: Monoid[AssetInfo] = new Monoid[AssetInfo] {
    override def empty: AssetInfo = AssetInfo(isReissuable = true, 0, None)
    override def combine(x: AssetInfo, y: AssetInfo): AssetInfo =
      AssetInfo(x.isReissuable && y.isReissuable, x.volume + y.volume, y.script.orElse(x.script))
  }
}

case class AssetDescription(issuer: PublicKeyAccount,
                            name: Array[Byte],
                            description: Array[Byte],
                            decimals: Int,
                            reissuable: Boolean,
                            totalVolume: BigInt,
                            script: Option[Script]) {
  override def equals(obj: scala.Any) = obj match {
    case o: AssetDescription =>
      o.issuer == this.issuer &&
        o.name.sameElements(name) &&
        o.description.sameElements(description) &&
        o.decimals == decimals &&
        o.reissuable == reissuable &&
        o.totalVolume == totalVolume &&
        o.script == script
    case _ => false
  }
}

case class AccountDataInfo(data: Map[String, DataEntry[_]])

object AccountDataInfo {
  implicit val accountDataInfoMonoid: Monoid[AccountDataInfo] = new Monoid[AccountDataInfo] {
    override def empty: AccountDataInfo = AccountDataInfo(Map.empty)

    override def combine(x: AccountDataInfo, y: AccountDataInfo): AccountDataInfo = AccountDataInfo(x.data ++ y.data)
  }
}

case class Diff(transactions: Map[ByteStr, (Int, Transaction, Set[Address])],
                portfolios: Map[Address, Portfolio],
                issuedAssets: Map[AssetId, AssetInfo],
                aliases: Map[Alias, Address],
                orderFills: Map[ByteStr, VolumeAndFee],
                leaseState: Map[ByteStr, Boolean],
                scripts: Map[Address, Option[Script]],
                accountData: Map[Address, AccountDataInfo]) {

  lazy val accountTransactionIds: Map[Address, List[ByteStr]] = {
    val map: List[(Address, Set[(Int, Long, ByteStr)])] = transactions.toList
      .flatMap { case (id, (h, tx, accs)) => accs.map(acc => acc -> Set((h, tx.timestamp, id))) }
    val groupedByAcc = map.foldLeft(Map.empty[Address, Set[(Int, Long, ByteStr)]]) {
      case (m, (acc, set)) =>
        m.combine(Map(acc -> set))
    }
    groupedByAcc
      .mapValues(l => l.toList.sortBy { case ((h, t, _)) => (-h, -t) }) // fresh head ([h=2, h=1, h=0])
      .mapValues(_.map(_._3))
  }
}

object Diff {
  def apply(height: Int,
            tx: Transaction,
            portfolios: Map[Address, Portfolio] = Map.empty,
            assetInfos: Map[AssetId, AssetInfo] = Map.empty,
            aliases: Map[Alias, Address] = Map.empty,
            orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
            leaseState: Map[ByteStr, Boolean] = Map.empty,
            scripts: Map[Address, Option[Script]] = Map.empty,
            accountData: Map[Address, AccountDataInfo] = Map.empty): Diff =
    Diff(
      transactions = Map((tx.id(), (height, tx, portfolios.keys.toSet))),
      portfolios = portfolios,
      issuedAssets = assetInfos,
      aliases = aliases,
      orderFills = orderFills,
      leaseState = leaseState,
      scripts = scripts,
      accountData = accountData
    )

  val empty = new Diff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff.empty

    override def combine(older: Diff, newer: Diff): Diff =
      Diff(
        transactions = older.transactions ++ newer.transactions,
        portfolios = older.portfolios.combine(newer.portfolios),
        issuedAssets = older.issuedAssets.combine(newer.issuedAssets),
        aliases = older.aliases ++ newer.aliases,
        orderFills = older.orderFills.combine(newer.orderFills),
        leaseState = older.leaseState ++ newer.leaseState,
        scripts = older.scripts ++ newer.scripts,
        accountData = older.accountData.combine(newer.accountData)
      )
  }
}
