package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit

import scala.annotation.tailrec

import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.{OptimisticLockable, ScorexLogging}
import kamon.Kamon
import kamon.metric.MeasurementUnit

final class UtxPriorityPool(base: Blockchain) extends ScorexLogging with OptimisticLockable {
  import UtxPriorityPool._

  @volatile private[this] var priorityDiffs         = Seq.empty[Diff]
  @volatile private[this] var priorityDiffsCombined = Monoid.combineAll(priorityDiffs)

  def priorityTransactions: Seq[Transaction] = priorityDiffs.flatMap(_.transactionsValues)
  def priorityTransactionIds: Seq[ByteStr]   = priorityDiffs.flatMap(_.transactions.keys)

  def compositeBlockchain: CompositeBlockchain = CompositeBlockchain(base, Some(priorityDiffsCombined))

  def lockedWrite[T](f: => T): T =
    this.writeLock(f)

  def optimisticRead[T](f: => T)(shouldRecheck: T => Boolean): T =
    this.readLockCond(f)(shouldRecheck)

  def addPriorityDiffs(discDiffs: Seq[Diff]): Unit = {
    if (discDiffs.nonEmpty) {
      discDiffs.filterNot(priorityDiffs.contains).foreach { diff =>
        diff.transactionsValues.foreach(PoolMetrics.addTransactionPriority(_))
        priorityDiffs :+= diff
        log.trace {
          val ids = diff.transactions.keys
          s"Priority diff ${diff.hashString} added: ${ids.mkString(", ")}"
        }
      }
      log.trace(s"Priority pool transactions order: ${priorityTransactionIds.mkString(", ")}")
      priorityDiffsCombined = Monoid.combineAll(priorityDiffs)
    }
  }

  def removeIds(removed: Set[ByteStr]): (Set[Transaction], Set[Transaction]) = {
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

    val result = removeRec(this.priorityDiffs)

    val txsToReset = result.resorted ++ result.removed
    txsToReset.foreach(PoolMetrics.removeTransactionPriority)

    priorityDiffs = result.diffsRest
    priorityDiffsCombined = Monoid.combineAll(priorityDiffs)

    (result.removed, result.resorted)
  }

  def transactionById(txId: ByteStr): Option[Transaction] =
    priorityDiffsCombined.transactions.get(txId).map(_.transaction)

  def contains(txId: ByteStr): Boolean = transactionById(txId).nonEmpty

  def pessimisticPortfolios(addr: Address): Seq[Portfolio] =
    for {
      diff    <- priorityDiffs
      (a, pf) <- diff.portfolios if a == addr
    } yield pf.pessimistic

  def nextMicroBlockSize(limit: Int): Int = {
    val sizes = priorityDiffs.map(_.transactions.size)

    sizes
      .scanLeft(0) { case (count, size) => count + size }
      .tail
      .takeWhile(_ <= limit)
      .lastOption
      .orElse(sizes.headOption)
      .getOrElse(limit)
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
    def contains(txId: ByteStr): Boolean     = diff.transactions.contains(txId)
    def transactionsValues: Seq[Transaction] = diff.transactions.values.map(_.transaction).toVector
    def transactionIds: collection.Set[ByteStr] = diff.transactions.keySet
  }
}
