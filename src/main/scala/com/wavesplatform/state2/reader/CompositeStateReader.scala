package com.wavesplatform.state2.reader

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.state2._
import scorex.account.{Account, Alias}
import scorex.transaction.Transaction
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.LeaseTransaction

class CompositeStateReader(inner: StateReader, blockDiff: BlockDiff) extends StateReader {

  val synchronizationToken: ReentrantReadWriteLock = inner.synchronizationToken

  private val txDiff = blockDiff.txsDiff

  override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] =
    txDiff.transactions.get(id)
      .map(t => (t._1, t._2))
      .orElse(inner.transactionInfo(id))

  override def accountPortfolio(a: Account): Portfolio =
    inner.accountPortfolio(a).combine(txDiff.portfolios.get(a).orEmpty)

  override def assetInfo(id: ByteArray): Option[AssetInfo] = (inner.assetInfo(id), txDiff.issuedAssets.get(id)) match {
    case (None, None) => None
    case (existing, upd) => Some(existing.orEmpty.combine(upd.orEmpty))
  }

  override def height: Int = inner.height + blockDiff.heightDiff

  override def accountTransactionIds(a: Account): Seq[ByteArray] = {
    val fromDiff = txDiff.accountTransactionIds.get(a).orEmpty
    fromDiff ++ inner.accountTransactionIds(a) // fresh head ++ stale tail
  }

  override def snapshotAtHeight(acc: Account, h: Int): Option[Snapshot] =
    blockDiff.snapshots.get(acc).flatMap(_.get(h)).orElse(inner.snapshotAtHeight(acc, h))

  override def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]
  = blockDiff.txsDiff.paymentTransactionIdsByHashes.get(hash)
    .orElse(inner.paymentTransactionIdByHash(hash))


  override def aliasesOfAddress(a: Account): Seq[Alias] =
    txDiff.aliases.filter(_._2 == a).keys.toSeq ++ inner.aliasesOfAddress(a)

  override def resolveAlias(a: Alias): Option[Account] = txDiff.aliases.get(a).orElse(inner.resolveAlias(a))

  override def getAssetIdByUniqueName(assetName: ByteArray): Option[ByteArray] = {
    txDiff.assetsWithUniqueNames.get(assetName).orElse(inner.getAssetIdByUniqueName(assetName))
  }

  override def findPreviousExchangeTxs(orderId: EqByteArray): Set[ExchangeTransaction] = {
    txDiff.previousExchangeTxs.get(orderId).orEmpty ++ inner.findPreviousExchangeTxs(orderId)
  }

  override def accountPortfolios: Map[Account, Portfolio] = Monoid.combine(inner.accountPortfolios, txDiff.portfolios)

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean =
    blockDiff.txsDiff.leaseState.getOrElse(EqByteArray(leaseTx.id), inner.isLeaseActive(leaseTx))

  override def activeLeases(): Seq[ByteArray] = {
    blockDiff.txsDiff.leaseState.collect { case (id, isActive) if isActive => id }.toSeq ++ inner.activeLeases()
  }

  override def lastUpdateHeight(acc: Account): Option[Int] = blockDiff.snapshots.get(acc).map(_.lastKey).orElse(inner.lastUpdateHeight(acc))

  override def containsTransaction(id: ByteArray): Boolean = blockDiff.txsDiff.transactions.contains(id) || inner.containsTransaction(id)

}

object CompositeStateReader {

  class Proxy(val inner: StateReader, blockDiff: () => BlockDiff) extends StateReader {

    override def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray] =
      new CompositeStateReader(inner, blockDiff()).paymentTransactionIdByHash(hash)

    override def aliasesOfAddress(a: Account): Seq[Alias] =
      new CompositeStateReader(inner, blockDiff()).aliasesOfAddress(a)

    override def accountPortfolio(a: Account): Portfolio =
      new CompositeStateReader(inner, blockDiff()).accountPortfolio(a)

    override def accountTransactionIds(a: Account): Seq[ByteArray] =
      new CompositeStateReader(inner, blockDiff()).accountTransactionIds(a)

    override def accountPortfolios: Map[Account, Portfolio] =
      new CompositeStateReader(inner, blockDiff()).accountPortfolios

    override def transactionInfo(id: ByteArray): Option[(Int, Transaction)] =
      new CompositeStateReader(inner, blockDiff()).transactionInfo(id)

    override def findPreviousExchangeTxs(orderId: EqByteArray): Set[ExchangeTransaction] =
      new CompositeStateReader(inner, blockDiff()).findPreviousExchangeTxs(orderId)

    override def resolveAlias(a: Alias): Option[Account] =
      new CompositeStateReader(inner, blockDiff()).resolveAlias(a)

    override def assetInfo(id: ByteArray): Option[AssetInfo] =
      new CompositeStateReader(inner, blockDiff()).assetInfo(id)

    override def height: Int =
      new CompositeStateReader(inner, blockDiff()).height

    override def isLeaseActive(leaseTx: LeaseTransaction): Boolean =
      new CompositeStateReader(inner, blockDiff()).isLeaseActive(leaseTx)

    override def activeLeases(): Seq[ByteArray] =
      new CompositeStateReader(inner, blockDiff()).activeLeases()

    override def getAssetIdByUniqueName(assetName: ByteArray): Option[ByteArray] =
      new CompositeStateReader(inner, blockDiff()).getAssetIdByUniqueName(assetName)

    override def lastUpdateHeight(acc: Account): Option[Int] =
      new CompositeStateReader(inner, blockDiff()).lastUpdateHeight(acc)

    override def snapshotAtHeight(acc: Account, h: Int): Option[Snapshot] =
      new CompositeStateReader(inner, blockDiff()).snapshotAtHeight(acc, h)

    override def containsTransaction(id: ByteArray): Boolean =
      new CompositeStateReader(inner, blockDiff()).containsTransaction(id)

    override val synchronizationToken: ReentrantReadWriteLock = inner.synchronizationToken
  }

}
