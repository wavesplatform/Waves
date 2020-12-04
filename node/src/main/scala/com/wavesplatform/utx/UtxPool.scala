package com.wavesplatform.utx

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.state.Portfolio
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utx.UtxPool.PackStrategy

import scala.concurrent.duration.FiniteDuration

trait UtxPool extends AutoCloseable {
  def putIfNew(tx: Transaction, forceValidate: Boolean = false): TracedResult[ValidationError, Boolean]
  def removeAll(txs: Iterable[Transaction]): Unit
  def spendableBalance(addr: Address, assetId: Asset): Long
  def pessimisticPortfolio(addr: Address): Portfolio
  def all: Seq[Transaction]
  def size: Int
  def transactionById(transactionId: ByteStr): Option[Transaction]
  def packUnconfirmed(
      rest: MultiDimensionalMiningConstraint,
      strategy: PackStrategy = PackStrategy.Unlimited,
      cancelled: () => Boolean = () => false
  ): (Option[Seq[Transaction]], MultiDimensionalMiningConstraint)
}

object UtxPool {
  sealed trait PackStrategy
  object PackStrategy {
    case class Limit(time: FiniteDuration)    extends PackStrategy
    case class Estimate(time: FiniteDuration) extends PackStrategy
    case object Unlimited                     extends PackStrategy
  }
}
