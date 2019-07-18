package com.wavesplatform.utx

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.state.Portfolio
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.script.trace.TracedResult

import scala.concurrent.duration.Duration

trait UtxPool extends AutoCloseable {
  def putIfNew(tx: Transaction, verify: Boolean = true): TracedResult[ValidationError, Boolean]

  def removeAll(txs: Traversable[Transaction]): Unit

  def pessimisticPortfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(rest: MultiDimensionalMiningConstraint, maxPackTime: Duration): (Seq[Transaction], MultiDimensionalMiningConstraint)
}
