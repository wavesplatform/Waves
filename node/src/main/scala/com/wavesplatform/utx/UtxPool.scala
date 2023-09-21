package com.wavesplatform.utx

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{MiningConstraint, MultiDimensionalMiningConstraint}
import com.wavesplatform.state.StateSnapshot
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utx.UtxPool.PackStrategy

import scala.concurrent.duration.FiniteDuration

trait UtxForAppender {
  def setPrioritySnapshots(snapshots: Seq[StateSnapshot]): Unit
}

trait UtxPool extends UtxForAppender with AutoCloseable {
  def putIfNew(tx: Transaction, forceValidate: Boolean = false): TracedResult[ValidationError, Boolean]
  def removeAll(txs: Iterable[Transaction]): Unit
  def all: Seq[Transaction]
  def size: Int
  def transactionById(transactionId: ByteStr): Option[Transaction]
  def addAndScheduleCleanup(transactions: Iterable[Transaction]): Unit
  def scheduleCleanup(): Unit
  def packUnconfirmed(
      rest: MultiDimensionalMiningConstraint,
      prevStateHash: Option[ByteStr],
      strategy: PackStrategy = PackStrategy.Unlimited,
      cancelled: () => Boolean = () => false
  ): (Option[Seq[Transaction]], MiningConstraint, Option[ByteStr])
  def resetPriorityPool(): Unit
  def cleanUnconfirmed(): Unit
  def getPriorityPool: Option[UtxPriorityPool]
}

object UtxPool {
  sealed trait PackStrategy
  object PackStrategy {
    case class Limit(time: FiniteDuration)    extends PackStrategy
    case class Estimate(time: FiniteDuration) extends PackStrategy
    case object Unlimited                     extends PackStrategy
  }
}
