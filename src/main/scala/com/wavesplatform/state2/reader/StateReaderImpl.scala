package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Address, Alias}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.{Transaction, TransactionParser}

class StateReaderImpl(p: StateStorage, val synchronizationToken: ReentrantReadWriteLock) extends SnapshotStateReader {

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = read { implicit l =>
    p.getTransaction(id).map {
      case (h, bytes) => (h, TransactionParser.parseBytes(bytes).get)
    }
  }

  override def accountPortfolio(a: Address): Portfolio = read { implicit l =>
    val waves = p.getWavesBalance(a).map { case (b, li, lo) => Portfolio(b, LeaseInfo(li, lo), Map.empty) }.orEmpty
    val assets = p.getAssetBalanceMap(a).map { f => Portfolio(0, LeaseInfo.empty, f) }.orEmpty

    Monoid.combine(waves, assets)
  }

  override def assetInfo(id: ByteStr): Option[AssetInfo] = read { implicit l =>
    p.getAssetInfo(id)
  }

  override def height: Int = read { implicit l => p.getHeight }

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = read { implicit l =>
    val totalRecords = p.getAccountTransactionsLengths(a).getOrElse(0)
    Range(Math.max(0, totalRecords - limit), totalRecords)
      .map(n => p.getAccountTransactionIds(a, n).get)
      .reverse
  }

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr] = read { implicit l =>
    p.getPaymentTransactionHashes(hash)
  }

  override def aliasesOfAddress(address: Address): Seq[Alias] = read { implicit l =>
    p.getAddressAliases(address).getOrElse(Seq.empty)
  }

  override def resolveAlias(alias: Alias): Option[Address] = read { implicit l =>
    p.getAddressOfAlias(alias)
  }

  override def accountPortfolios: Map[Address, Portfolio] = read { implicit l =>
    p.allPortfolios
  }

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = read { implicit l =>
    p.getLeaseState(leaseTx.id()).getOrElse(false)
  }

  override def activeLeases(): Seq[ByteStr] = read { implicit l =>
    p.getActiveLeases.getOrElse(Seq.empty[ByteStr])
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = read { implicit l =>
    p.getLastBalanceSnapshotHeight(acc)
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = read { implicit l =>
    p.getBalanceSnapshots(acc, h).map { case (ph, b, eb) => Snapshot(ph, b, eb) }
  }

  override def containsTransaction(id: ByteStr): Boolean = read { implicit l =>
    p.getTransaction(id).isDefined
  }

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo = read { implicit l =>
    p.getOrderFills(orderId).getOrElse(Monoid[OrderFillInfo].empty)
  }

  override def wavesBalance(a: Address): (Long, LeaseInfo) = read { implicit l =>
    p.getWavesBalance(a).map { case (v1, v2, v3) => (v1, LeaseInfo(v2, v3)) }.getOrElse((0L, LeaseInfo(0L, 0L)))
  }

  override def assetBalance(a: Address, asset: ByteStr): Long = read { implicit l =>
    p.getAssetBalance(a, asset).getOrElse(0L)
  }
}
