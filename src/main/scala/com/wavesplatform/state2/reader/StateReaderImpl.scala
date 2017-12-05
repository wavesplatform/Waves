package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Address, Alias}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.{Transaction, TransactionParser}

import scala.collection.JavaConverters._

class StateReaderImpl(p: StateStorage, val synchronizationToken: ReentrantReadWriteLock) extends SnapshotStateReader {

  val sp = Synchronized(p)

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = read { implicit l =>
    Option(sp().transactions.get(id)).map {
      case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
    }
  }

  override def accountPortfolio(a: Address): Portfolio = read { implicit l =>
    val waves = Option(sp().wavesBalance.get(a.bytes)).map { case (b, li, lo) => Portfolio(b, LeaseInfo(li, lo), Map.empty) }.orEmpty
    val assets = Portfolio(0, LeaseInfo.empty, sp().assetBalance.getMap(a.bytes))
    Monoid.combine(waves, assets)
  }

  override def assetInfo(id: ByteStr): Option[AssetInfo] = read { implicit l =>
    Option(sp().assets.get(id)).map {
      case (is, amt) => AssetInfo(is, amt)
    }
  }

  override def height: Int = read { implicit l => sp().getHeight }

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = read { implicit l =>
    val totalRecords = sp().accountTransactionsLengths.getOrDefault(a.bytes, 0)
    Range(Math.max(0, totalRecords - limit), totalRecords)
      .map(n => sp().accountTransactionIds.get(StateStorage.accountIndexKey(a, n)))
      .reverse
  }

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr] = read { implicit l =>
    Option(sp().paymentTransactionHashes.get(hash))
  }

  override def aliasesOfAddress(a: Address): Seq[Alias] = read { implicit l =>
    sp().aliasToAddress.asScala
      .collect { case (aliasName, addressBytes) if addressBytes == a.bytes =>
        Alias.buildWithCurrentNetworkByte(aliasName).explicitGet()
      }.toSeq
  }

  override def resolveAlias(a: Alias): Option[Address] = read { implicit l =>
    Option(sp().aliasToAddress.get(a.name))
      .map(b => Address.fromBytes(b.arr).explicitGet())
  }

  override def accountPortfolios: Map[Address, Portfolio] = read { implicit l =>
    val waves = sp()
      .wavesBalance.asScala.toMap.map { case (k, (b, li, lo)) => Address.fromBytes(k.arr).explicitGet() -> Portfolio(b, LeaseInfo(li, lo), Map.empty) }
    val assets = sp().assetBalance.all()
      .map { case (k, v) => Address.fromBytes(k.arr).explicitGet() -> Portfolio(0, LeaseInfo.empty, v) }
    Monoid.combine(assets, waves)
  }

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = read { implicit l =>
    sp().leaseState.getOrDefault(leaseTx.id(), false)
  }

  override def activeLeases(): Seq[ByteStr] = read { implicit l =>
    sp().leaseState
      .asScala
      .collect { case (leaseId, isActive) if isActive => leaseId }
      .toSeq
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = read { implicit l =>
    Option(sp().lastBalanceSnapshotHeight.get(acc.bytes))
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = read { implicit l =>
    Option(sp().balanceSnapshots.get(StateStorage.accountIndexKey(acc, h)))
      .map { case (ph, b, eb) => Snapshot(ph, b, eb) }
  }

  override def containsTransaction(id: ByteStr): Boolean = read { implicit l =>
    sp().transactions.containsKey(id)
  }

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo = read { _ =>
    Option(p.orderFills.get(orderId)).map(oi => OrderFillInfo(oi._1, oi._2)).orEmpty
  }

  override def wavesBalance(a: Address): (Long, LeaseInfo) =
    Option(p.wavesBalance.get(a.bytes))
      .map { case (v1, v2, v3) => (v1, LeaseInfo(v2, v3)) }
      .getOrElse((0L, LeaseInfo(0L, 0L)))

  override def assetBalance(a: Address, asset: ByteStr): Long = p.assetBalance.get(a.bytes, asset).getOrElse(0L)
}
