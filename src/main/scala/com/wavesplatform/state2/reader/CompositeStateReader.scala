package com.wavesplatform.state2.reader

import cats.implicits._
import cats.kernel.Monoid
import com.wavesplatform.state2._
import scorex.account.{Account, Alias}
import scorex.transaction.Transaction
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}

class CompositeStateReader(inner: StateReader, blockDiff: BlockDiff) extends StateReader {
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
    blockDiff.updates.get(acc).flatMap(_.get(h)).orElse(inner.snapshotAtHeight(acc, h))

  override def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]
  = blockDiff.txsDiff.paymentTransactionIdsByHashes.get(hash)
    .orElse(inner.paymentTransactionIdByHash(hash))


  override def aliasesOfAddress(a: Account): Seq[Alias] =
    txDiff.aliases.filter(_._2 == a).keys.toSeq ++ inner.aliasesOfAddress(a)

  override def resolveAlias(a: Alias): Option[Account] = txDiff.aliases.get(a).orElse(inner.resolveAlias(a))

  override def findPreviousExchangeTxs(orderId: EqByteArray): Set[ExchangeTransaction] = {
    val newEtxs = txDiff.transactions
      .collect { case (_, (_, ets: ExchangeTransaction, _)) => ets }
      .filter(etx => (etx.buyOrder.id sameElements orderId.arr) || (etx.sellOrder.id sameElements orderId.arr))
      .toSet
    newEtxs ++ inner.findPreviousExchangeTxs(orderId)
  }

  override def accountPortfolios: Map[Account, Portfolio] = Monoid.combine(inner.accountPortfolios, txDiff.portfolios)

  override def isLeaseActive(leaseTx: LeaseTransaction): Boolean = {
    val innerActive = inner.isLeaseActive(leaseTx)
    val diffActive = blockDiff.txsDiff.transactions.keys.exists(_ == EqByteArray(leaseTx.id))
    val diffCancelExists = blockDiff.txsDiff.transactions.values.map(_._2)
      .filter(_.isInstanceOf[LeaseCancelTransaction])
      .map(_.asInstanceOf[LeaseCancelTransaction])
      .exists(_.leaseId sameElements leaseTx.id)

    (innerActive || diffActive) && !diffCancelExists
  }

  override def activeLeases(): Seq[ByteArray] = {
    blockDiff.txsDiff.effectiveLeaseTxUpdates._1.toSeq ++ inner.activeLeases()
  }

  override def lastUpdateHeight(acc: Account): Option[Int] = blockDiff.updates.get(acc).flatMap(_.keySet.maximumOption).orElse(inner.lastUpdateHeight(acc))
}

object CompositeStateReader {
  def proxy(inner: StateReader, blockDiff: () => BlockDiff): StateReader = new StateReader {

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

    override def lastUpdateHeight(acc: Account): Option[Int] =
      new CompositeStateReader(inner, blockDiff()).lastUpdateHeight(acc)

    override def snapshotAtHeight(acc: Account, h: Int): Option[Snapshot] =
      new CompositeStateReader(inner, blockDiff()).snapshotAtHeight(acc, h)
  }
}