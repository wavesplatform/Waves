package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.state2._
import monix.eval.Coeval
import scorex.account.{Address, Alias}
import scorex.transaction.Transaction
import scorex.transaction.lease.LeaseTransaction

class CompositeStateReader(inner: StateReader, blockDiff: BlockDiff) extends StateReader {

  def synchronizationToken: ReentrantReadWriteLock = inner.synchronizationToken

  private val txDiff = blockDiff.txsDiff

  override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
    txDiff.transactions.get(id)
      .map(t => (t._1, t._2))
      .orElse(inner.transactionInfo(id))

  override def accountPortfolio(a: Address): Portfolio =
    inner.accountPortfolio(a).combine(txDiff.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteStr): Option[AssetInfo] = (inner.assetInfo(id), txDiff.issuedAssets.get(id)) match {
    case (None, None) => None
    case (existing, upd) => Some(existing.orEmpty.combine(upd.orEmpty))
  }

  override def height: Int = inner.height + blockDiff.heightDiff

  override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] = {
    val fromDiff = txDiff.accountTransactionIds.get(a).orEmpty
    if (fromDiff.length >= limit) {
      fromDiff.take(limit)
    } else {
      fromDiff ++ inner.accountTransactionIds(a, limit - fromDiff.size) // fresh head ++ stale tail
    }
  }

  override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] =
    blockDiff.snapshots.get(acc).flatMap(_.get(h)).orElse(inner.snapshotAtHeight(acc, h))

  override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr]
  = blockDiff.txsDiff.paymentTransactionIdsByHashes.get(hash)
    .orElse(inner.paymentTransactionIdByHash(hash))


  override def aliasesOfAddress(a: Address): Seq[Alias] =
    txDiff.aliases.filter(_._2 == a).keys.toSeq ++ inner.aliasesOfAddress(a)

  override def resolveAlias(a: Alias): Option[Address] = txDiff.aliases.get(a).orElse(inner.resolveAlias(a))

  override def accountPortfolios: Map[Address, Portfolio] = Monoid.combine(inner.accountPortfolios, txDiff.portfolios)

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean =
    blockDiff.txsDiff.leaseState.getOrElse(leaseTx.id, inner.isLeaseActive(leaseTx))

  override def activeLeases(): Seq[ByteStr] = {
    blockDiff.txsDiff.leaseState.collect { case (id, isActive) if isActive => id }.toSeq ++ inner.activeLeases()
  }

  override def lastUpdateHeight(acc: Address): Option[Int] = blockDiff.snapshots.get(acc).map(_.lastKey).orElse(inner.lastUpdateHeight(acc))

  override def containsTransaction(id: ByteStr): Boolean = blockDiff.txsDiff.transactions.contains(id) || inner.containsTransaction(id)

  override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo =
    blockDiff.txsDiff.orderFills.get(orderId).orEmpty.combine(inner.filledVolumeAndFee(orderId))

  override def wavesBalance(a: Address): (Long, LeaseInfo) = {
    val in = inner.partialPortfolio(a)
    val diffed = blockDiff.txsDiff.portfolios.get(a).orEmpty
    (in.balance + diffed.balance, Monoid.combine(diffed.leaseInfo, in.leaseInfo))
  }

  override def assetBalance(a: Address, asset: ByteStr): Long = {
    val in: Long = inner.assetBalance(a, asset)
    val diffed: Long = blockDiff.txsDiff.portfolios.get(a).orEmpty.assets.getOrElse(asset, 0)
    in + diffed
  }
}

object CompositeStateReader {

  class Proxy(val inner: Coeval[StateReader], blockDiff: Coeval[BlockDiff]) extends StateReader {

    override def toString: String = {
      val bd = blockDiff()
      s"proxy((dh=${bd.heightDiff}, dtx=${bd.txsDiff.transactions.values.map(_._2.transactionType)} | $inner())"
    }

    override def synchronizationToken: ReentrantReadWriteLock = inner().synchronizationToken

    override def paymentTransactionIdByHash(hash: ByteStr): Option[ByteStr] =
      new CompositeStateReader(inner(), blockDiff()).paymentTransactionIdByHash(hash)

    override def aliasesOfAddress(a: Address): Seq[Alias] =
      new CompositeStateReader(inner(), blockDiff()).aliasesOfAddress(a)

    override def accountPortfolio(a: Address): Portfolio =
      new CompositeStateReader(inner(), blockDiff()).accountPortfolio(a)

    override def accountTransactionIds(a: Address, limit: Int): Seq[ByteStr] =
      new CompositeStateReader(inner(), blockDiff()).accountTransactionIds(a, limit)

    override def accountPortfolios: Map[Address, Portfolio] =
      new CompositeStateReader(inner(), blockDiff()).accountPortfolios

    override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] =
      new CompositeStateReader(inner(), blockDiff()).transactionInfo(id)

    override def resolveAlias(a: Alias): Option[Address] =
      new CompositeStateReader(inner(), blockDiff()).resolveAlias(a)

    override def assetInfo(id: ByteStr): Option[AssetInfo] =
      new CompositeStateReader(inner(), blockDiff()).assetInfo(id)

    override def height: Int =
      new CompositeStateReader(inner(), blockDiff()).height

    override def isLeaseActive(leaseTx: LeaseTransaction): Boolean =
      new CompositeStateReader(inner(), blockDiff()).isLeaseActive(leaseTx)

    override def activeLeases(): Seq[ByteStr] =
      new CompositeStateReader(inner(), blockDiff()).activeLeases()

    override def lastUpdateHeight(acc: Address): Option[Int] =
      new CompositeStateReader(inner(), blockDiff()).lastUpdateHeight(acc)

    override def snapshotAtHeight(acc: Address, h: Int): Option[Snapshot] =
      new CompositeStateReader(inner(), blockDiff()).snapshotAtHeight(acc, h)

    override def containsTransaction(id: ByteStr): Boolean =
      new CompositeStateReader(inner(), blockDiff()).containsTransaction(id)

    override def filledVolumeAndFee(orderId: ByteStr): OrderFillInfo =
      new CompositeStateReader(inner(), blockDiff()).filledVolumeAndFee(orderId)

    override def wavesBalance(a: Address): (Long, LeaseInfo) =
      new CompositeStateReader(inner(), blockDiff()).wavesBalance(a)

    override def assetBalance(a: Address, asset: ByteStr): Long =
      new CompositeStateReader(inner(), blockDiff()).assetBalance(a, asset)
  }

  def composite(inner: Coeval[StateReader], blockDiff: Coeval[BlockDiff]): Proxy = new Proxy(inner, blockDiff)
}
