package com.wavesplatform.utx

import java.time.Duration
import java.time.temporal.ChronoUnit
import java.util.concurrent.locks.StampedLock

import cats.kernel.Monoid
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{Blockchain, Diff, Portfolio}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import kamon.Kamon
import kamon.metric.MeasurementUnit

final class UtxPriorityPool(base: Blockchain) extends ScorexLogging {
  private[this] val lock = new StampedLock

  @volatile private[this] var priorityDiffs         = Vector.empty[Diff]
  @volatile private[this] var priorityDiffsCombined = Monoid.combineAll(priorityDiffs)

  def priorityTransactions: Seq[Transaction] = priorityDiffs.flatMap(_.transactionsValues)
  def priorityTransactionIds: Seq[ByteStr]   = priorityDiffs.flatMap(_.transactions.keys)

  def compositeBlockchain: CompositeBlockchain = CompositeBlockchain(base, Some(priorityDiffsCombined))

  def lockedWrite[T](f: => T): T = {
    val stamp = lock.writeLock()
    try {
      f
    } finally {
      lock.unlockWrite(stamp)
    }
  }

  def optimisticRead[T](f: => T)(shouldRecheck: T => Boolean): T = {
    def lockedRead(): T = {
      val stamp = lock.readLock()
      try {
        f
      } finally {
        lock.unlockRead(stamp)
      }
    }

    val stamp = lock.tryOptimisticRead()
    if (stamp != 0) {
      val value = f
      if (lock.validate(stamp) || !shouldRecheck(value)) value
      else lockedRead()
    } else lockedRead()
  }

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
    val (diffsToReset, diffsToKeep) = priorityDiffs.partition(pd => removed.exists(pd.contains))
    val factRemoved                 = Set.newBuilder[Transaction]
    val notRemoved                  = Set.newBuilder[Transaction]

    diffsToReset.foreach { diff =>
      val fullyReset: Boolean = diff.transactions.keySet.forall(removed)
      diff.transactionsValues.foreach { tx =>
        PoolMetrics.removeTransactionPriority(tx)
        factRemoved += tx
      }
      if (!fullyReset) {
        val txsToAdd = diff.transactions.view.filterKeys(!removed(_)).values.map(_.transaction)
        log.warn {
          val added      = txsToAdd.map(_.id())
          val removedIds = diff.transactions.keySet.intersect(removed)
          s"Resetting diff ${diff.hashString} partially: removed = [${removedIds.mkString(", ")}], resorted = [${added.mkString(", ")}]"
        }
        notRemoved ++= txsToAdd
      }
    }
    priorityDiffs = diffsToKeep
    priorityDiffsCombined = Monoid.combineAll(priorityDiffs)

    (factRemoved.result(), notRemoved.result())
  }

  def transactionById(txId: ByteStr): Option[Transaction] =
    priorityDiffsCombined.transactions.get(txId).map(_.transaction)

  def contains(txId: ByteStr): Boolean = transactionById(txId).nonEmpty

  def pessimisticPortfolios(addr: Address): Seq[Portfolio] =
    for {
      diff    <- priorityDiffs
      (a, pf) <- diff.portfolios if a == addr
    } yield pf.pessimistic

  def nextMicroBlockSize(): Option[Int] = {
    priorityDiffs.headOption.map(_.transactions.size)
  }

  private[this] implicit class DiffExt(diff: Diff) {
    def contains(txId: ByteStr): Boolean     = diff.transactions.contains(txId)
    def transactionsValues: Seq[Transaction] = diff.transactions.values.map(_.transaction).toVector
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
