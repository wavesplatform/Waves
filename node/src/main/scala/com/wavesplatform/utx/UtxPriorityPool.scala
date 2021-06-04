package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit

import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}
import kamon.Kamon
import kamon.metric.MeasurementUnit

import scala.annotation.tailrec

final class UtxPriorityPool(base: Blockchain) extends ScorexLogging with OptimisticLockable {
  import UtxPriorityPool._

  private[this] case class PriorityData(diff: Diff, isValid: Boolean = true)

  @volatile private[this] var priorityDiffs         = Seq.empty[PriorityData]
  @volatile private[this] var priorityDiffsCombined = Diff.empty

  def validPriorityDiffs: Seq[Diff]          = priorityDiffs.collect { case PriorityData(diff, true) => diff }
  def priorityTransactions: Seq[Transaction] = priorityDiffs.flatMap(_.diff.transactionsValues)
  def priorityTransactionIds: Seq[ByteStr]   = priorityTransactions.map(_.id())

  def compositeBlockchain: CompositeBlockchain = CompositeBlockchain(base, priorityDiffsCombined)

  def lockedWrite[T](f: => T): T =
    this.writeLock(f)

  def optimisticRead[T](f: => T)(shouldRecheck: T => Boolean): T =
    this.readLockCond(f)(shouldRecheck)

  private[utx] def setPriorityDiffs(discDiffs: Seq[Diff]): Set[Transaction] =
    if (discDiffs.isEmpty) Set.empty
    else {
      val transactions = updateDiffs(_ => discDiffs.map(PriorityData(_)))
      log.trace(
        s"Priority pool updated with diffs: [${discDiffs.map(_.hashString).mkString(", ")}], transactions order: [${priorityTransactionIds.mkString(", ")}]"
      )
      transactions
    }

  private[utx] def invalidateTxs(removed: Set[ByteStr]): Unit =
    updateDiffs(_.map { pd =>
      if (pd.diff.transactionIds.exists(removed)) {
        val keep = pd.diff.transactions.filterNot { case (id, _) => removed(id) }
        pd.copy(pd.diff.copy(keep), isValid = false)
      } else pd
    })

  private[utx] def removeIds(removed: Set[ByteStr]): Set[Transaction] = {
    case class RemoveResult(diffsRest: Seq[PriorityData], removed: Set[Transaction])

    @tailrec
    def removeRec(diffs: Seq[PriorityData], cleanRemoved: Set[Transaction] = Set.empty): RemoveResult = diffs match {
      case Nil =>
        RemoveResult(Nil, cleanRemoved)

      case pd +: rest if pd.diff.transactionIds.subsetOf(removed) =>
        removeRec(rest, cleanRemoved ++ pd.diff.transactionsValues)

      case _ if cleanRemoved.map(_.id()) == removed =>
        RemoveResult(diffs, cleanRemoved)

      case _ => // Partial remove, invalidate priority pool
        RemoveResult(diffs.map(_.copy(isValid = false)), cleanRemoved)
    }

    val result = removeRec(this.priorityDiffs)
    if (result.removed.nonEmpty)
      log.trace(
        s"Removing diffs from priority pool: removed txs: [${result.removed.map(_.id()).mkString(", ")}], remaining diffs: [${result.diffsRest.map(_.diff.hashString).mkString(", ")}]"
      )

    updateDiffs(_ => result.diffsRest)
    if (priorityTransactionIds.nonEmpty) log.trace(s"Priority pool transactions order: ${priorityTransactionIds.mkString(", ")}")

    result.removed
  }

  def transactionById(txId: ByteStr): Option[Transaction] =
    priorityDiffsCombined.transactions.get(txId).map(_.transaction)

  def contains(txId: ByteStr): Boolean = transactionById(txId).nonEmpty

  def pessimisticPortfolios(addr: Address): Seq[Portfolio] =
    for {
      diff    <- validPriorityDiffs
      (a, pf) <- diff.portfolios if a == addr
    } yield pf.pessimistic

  def nextMicroBlockSize(limit: Int): Int = {
    @tailrec
    def nextMicroBlockSizeRec(last: Int, diffs: Seq[Diff]): Int = diffs match {
      case Nil => last.max(limit)
      case diff +: _ if last + diff.transactions.size > limit =>
        if (last == 0) diff.transactions.size // First micro
        else last
      case diff +: rest => nextMicroBlockSizeRec(last + diff.transactions.size, rest)
    }
    nextMicroBlockSizeRec(0, priorityDiffs.map(_.diff))
  }

  private[utx] def clear(): Seq[Transaction] = {
    val txs = this.priorityTransactions
    updateDiffs(_ => Nil)
    txs
  }

  private[this] def updateDiffs(f: Seq[PriorityData] => Seq[PriorityData]): Set[Transaction] = {
    val oldTxs = priorityTransactions.toSet

    priorityDiffs = f(priorityDiffs).filterNot(_.diff.transactions.isEmpty)
    priorityDiffsCombined = Monoid.combineAll(validPriorityDiffs)

    val newTxs = priorityTransactions.toSet

    val removed = oldTxs diff newTxs
    removed.foreach(PoolMetrics.removeTransactionPriority)
    (newTxs diff oldTxs).foreach(PoolMetrics.addTransactionPriority)
    removed
  }

  //noinspection TypeAnnotation
  private[this] object PoolMetrics {
    private[this] val SampleInterval: Duration = Duration.of(500, ChronoUnit.MILLIS)

    private[this] val prioritySizeStats = Kamon.rangeSampler("utx.priority-pool-size", MeasurementUnit.none, SampleInterval).withoutTags()
    private[this] val priorityBytesStats =
      Kamon.rangeSampler("utx.priority-pool-bytes", MeasurementUnit.information.bytes, SampleInterval).withoutTags()

    def addTransactionPriority(tx: Transaction): Unit = {
      prioritySizeStats.increment()
      priorityBytesStats.increment(tx.bytes().length)
    }

    def removeTransactionPriority(tx: Transaction): Unit = {
      prioritySizeStats.decrement()
      priorityBytesStats.decrement(tx.bytes().length)
    }
  }
}

private object UtxPriorityPool {
  implicit class DiffExt(private val diff: Diff) extends AnyVal {
    def contains(txId: ByteStr): Boolean        = diff.transactions.contains(txId)
    def transactionsValues: Seq[Transaction]    = diff.transactions.values.map(_.transaction).toVector
    def transactionIds: collection.Set[ByteStr] = diff.transactions.keySet
  }
}
