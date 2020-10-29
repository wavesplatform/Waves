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

  def compositeBlockchain: CompositeBlockchain = CompositeBlockchain(base, Some(priorityDiffsCombined))

  def lockedWrite[T](f: => T): T =
    this.writeLock(f)

  def optimisticRead[T](f: => T)(shouldRecheck: T => Boolean): T =
    this.readLockCond(f)(shouldRecheck)

  private[utx] def setPriorityDiffs(discDiffs: Seq[Diff]): Seq[Transaction] = {
    if (discDiffs.isEmpty) return Nil

    val transactions = this.clear()
    priorityDiffs = discDiffs.map(PriorityData(_))
    priorityDiffsCombined = Monoid.combineAll(validPriorityDiffs)

    discDiffs.foreach { diff =>
      diff.transactionsValues.foreach(PoolMetrics.addTransactionPriority(_))
      log.trace(s"Priority diff ${diff.hashString} added: ${diff.transactions.keys.mkString(", ")}")
    }

    log.trace(s"Priority pool transactions order: ${priorityTransactionIds.mkString(", ")}")
    transactions.filterNot(tx => discDiffs.exists(_.contains(tx.id())))
  }

  private[utx] def invalidateTxs(removed: Set[ByteStr]): Unit = {
    priorityDiffs = priorityDiffs
      .map { pd =>
        if (pd.diff.transactionIds.exists(removed)) {
          val (drop, keep) = pd.diff.transactions.partition { case (id, _) => removed(id) }
          drop.foreach { case (_, txi) => PoolMetrics.removeTransactionPriority(txi.transaction) }
          pd.copy(pd.diff.copy(keep), isValid = false)
        } else pd
      }
      .filterNot(_.diff.transactions.isEmpty)
    priorityDiffsCombined = Monoid.combineAll(validPriorityDiffs)
  }

  private[utx] def removeIds(removed: Set[ByteStr]): (Set[Transaction], Set[Transaction]) = {
    case class RemoveResult(diffsRest: Seq[Diff], removed: Set[Transaction], resorted: Set[Transaction])

    @tailrec
    def removeRec(diffs: Seq[Diff], cleanRemoved: Set[Transaction] = Set.empty): RemoveResult = diffs match {
      case Nil =>
        RemoveResult(Nil, cleanRemoved, Set.empty)

      case diff +: rest if diff.transactionIds.subsetOf(removed) =>
        removeRec(rest, cleanRemoved ++ diff.transactionsValues)

      case _ if cleanRemoved.map(_.id()) == removed =>
        RemoveResult(diffs, cleanRemoved, Set.empty)

      case _ => // Partial remove, drop entire priority pool
        val (restRemoved, resorted) = diffs.flatMap(_.transactionsValues).partition(tx => removed(tx.id()))
        RemoveResult(Nil, cleanRemoved ++ restRemoved, resorted.toSet)
    }

    val result = removeRec(this.priorityDiffs.map(_.diff))
    log.trace(s"Removing diffs from priority pool: resorted txs: [${result.resorted.map(_.id()).mkString(", ")}], removed txs: [${result.removed
      .map(_.id())
      .mkString(", ")}], remaining diffs: [${result.diffsRest.map(_.hashString).mkString(", ")}]")

    val txsToReset = result.resorted ++ result.removed
    txsToReset.foreach(PoolMetrics.removeTransactionPriority)

    priorityDiffs = result.diffsRest.map(PriorityData(_))
    priorityDiffsCombined = Monoid.combineAll(validPriorityDiffs)
    log.trace(s"Priority pool transactions order: ${priorityTransactionIds.mkString(", ")}")

    (result.removed, result.resorted)
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
    priorityDiffsCombined = Diff.empty
    priorityDiffs = Nil
    log.trace("Priority pool cleared")
    txs.foreach(PoolMetrics.removeTransactionPriority)
    txs
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
