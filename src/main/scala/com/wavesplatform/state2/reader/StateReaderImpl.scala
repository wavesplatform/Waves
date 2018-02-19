package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats._
import cats.implicits._
import com.wavesplatform.state2._
import scorex.account.{Address, Alias}
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.Script
import scorex.transaction.{Transaction, TransactionParser}

class StateReaderImpl(p: StateStorage, val synchronizationToken: ReentrantReadWriteLock) extends SnapshotStateReader {

  val sp = Synchronized(p)

  override def transactionInfo(id: ByteStr): Option[(Int, Option[Transaction])] = read { implicit l =>
    sp().getTransaction(id).map {
      case (h, bytes) => (h, if (bytes.length == 0) None else TransactionParser.parseBytes(bytes).toOption)
    }
  }

  override def accountPortfolio(a: Address): Portfolio = read { implicit l =>
    val waves = sp().getWavesBalance(a).map { case (b, li, lo) => Portfolio(b, LeaseInfo(li, lo), Map.empty) }.orEmpty
    val assets = sp().getAssetBalanceMap(a).map { f => Portfolio(0, LeaseInfo.empty, f) }.orEmpty

    Monoid.combine(waves, assets)
  }

  override def assetInfo(id: ByteStr): Option[AssetInfo] = read { implicit l =>
    sp().getAssetInfo(id)
  }

  override def height: Int = read { implicit l => sp().getHeight }

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = read { implicit l =>
    val totalRecords = sp().getAccountTransactionsLengths(a).getOrElse(0)
    Range(Math.max(0, totalRecords - limit), totalRecords)
      .map(n => sp().getAccountTransactionIds(a, n).get)
      .reverse
  }

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr] = read { implicit l =>
    sp().getPaymentTransactionHashes(hash)
  }

  override def aliasesOfAddress(address: Address): Seq[Alias] = read { implicit l =>
    sp().getAddressAliases(address).getOrElse(Seq.empty)
  }

  override def resolveAlias(alias: Alias): Option[Address] = read { implicit l =>
    sp().getAddressOfAlias(alias)
  }

  override def accountPortfolios: Map[Address, Portfolio] = read { implicit l =>
    sp().allPortfolios
  }

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = read { implicit l =>
    sp().getLeaseState(leaseTx.id()).getOrElse(false)
  }

  override def activeLeases(): Seq[ByteStr] = read { implicit l =>
    sp().getActiveLeases.getOrElse(Seq.empty[ByteStr])
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = read { implicit l =>
    sp().getLastBalanceSnapshotHeight(acc)
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] = read { implicit l =>
    sp().getBalanceSnapshots(acc, h).map { case (ph, b, eb) => Snapshot(ph, b, eb) }
  }

  override def containsTransaction(id: ByteStr): Boolean = read { implicit l =>
    sp().getTransaction(id).isDefined
  }

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo = read { implicit l =>
    sp().getOrderFills(orderId).getOrElse(Monoid[OrderFillInfo].empty)
  }

  override def wavesBalance(a: Address): (Long, LeaseInfo) = read { implicit l =>
    sp().getWavesBalance(a).map { case (v1, v2, v3) => (v1, LeaseInfo(v2, v3)) }.getOrElse((0L, LeaseInfo(0L, 0L)))
  }

  override def accountScript(address: Address): Option[Script] = read { implicit l =>
    sp().getScript(address)
  }
  override def assetBalance(a: Address, asset: ByteStr): Long = read { implicit l =>
    sp().getAssetBalance(a, asset).getOrElse(0L)
  }
}
