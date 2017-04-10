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

  override def nonEmptyAccounts: Seq[Account] =
    inner.nonEmptyAccounts ++ txDiff.portfolios.keySet

  override def accountTransactionIds(a: Account): Seq[ByteArray] = {
    val fromDiff = txDiff.accountTransactionIds.get(a).orEmpty
    fromDiff ++ inner.accountTransactionIds(a) // fresh head ++ stale tail
  }

  override def effectiveBalanceAtHeightWithConfirmations(acc: Account, height: Int, confs: Int): Long = {
    val localEffectiveBalanceSnapshotsOfAccount = blockDiff.effectiveBalanceSnapshots
      .filter(ebs => ebs.acc == acc)

    lazy val relatedUpdates = localEffectiveBalanceSnapshotsOfAccount.filter(_.height > height - confs)
    lazy val storedEffectiveBalance = inner.effectiveBalanceAtHeightWithConfirmations(acc, height - blockDiff.heightDiff, confs - blockDiff.heightDiff)

    if (localEffectiveBalanceSnapshotsOfAccount.isEmpty)
      storedEffectiveBalance
    else {
      if (confs < blockDiff.heightDiff) {
        relatedUpdates.headOption match {
          case None =>
            localEffectiveBalanceSnapshotsOfAccount.maxBy(_.height).effectiveBalance
          case Some(relatedUpdate) =>
            Math.min(relatedUpdate.prevEffectiveBalance, relatedUpdates.map(_.effectiveBalance).min)
        }
      }
      else {
        val localMin = localEffectiveBalanceSnapshotsOfAccount.map(_.effectiveBalance).min
        val prevEffBalance = if (inner.height == 0)
          localEffectiveBalanceSnapshotsOfAccount.minBy(_.height).prevEffectiveBalance
        else
          storedEffectiveBalance
        Math.min(prevEffBalance, localMin)

      }
    }
  }

  override def paymentTransactionIdByHash(hash: ByteArray): Option[ByteArray]
  = blockDiff.txsDiff.paymentTransactionIdsByHashes.get(hash)
    .orElse(inner.paymentTransactionIdByHash(hash))

  override def maxPaymentTransactionTimestampInPreviousBlocks(a: Account): Option[Long] = {
    blockDiff.maxPaymentTransactionTimestamp.get(a)
      .orElse(inner.maxPaymentTransactionTimestampInPreviousBlocks(a))
  }

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

    override def effectiveBalanceAtHeightWithConfirmations(acc: Account, height: Int, confs: Int): Long =
      new CompositeStateReader(inner, blockDiff()).effectiveBalanceAtHeightWithConfirmations(acc, height, confs)

    override def nonEmptyAccounts: Seq[Account] =
      new CompositeStateReader(inner, blockDiff()).nonEmptyAccounts

    override def findPreviousExchangeTxs(orderId: EqByteArray): Set[ExchangeTransaction] =
      new CompositeStateReader(inner, blockDiff()).findPreviousExchangeTxs(orderId)

    override def resolveAlias(a: Alias): Option[Account] =
      new CompositeStateReader(inner, blockDiff()).resolveAlias(a)

    override def assetInfo(id: ByteArray): Option[AssetInfo] =
      new CompositeStateReader(inner, blockDiff()).assetInfo(id)

    override def height: Int =
      new CompositeStateReader(inner, blockDiff()).height

    override def maxPaymentTransactionTimestampInPreviousBlocks(a: Account): Option[Long] =
      new CompositeStateReader(inner, blockDiff()).maxPaymentTransactionTimestampInPreviousBlocks(a)

    override def isLeaseActive(leaseTx: LeaseTransaction): Boolean =
      new CompositeStateReader(inner, blockDiff()).isLeaseActive(leaseTx)
  }
}