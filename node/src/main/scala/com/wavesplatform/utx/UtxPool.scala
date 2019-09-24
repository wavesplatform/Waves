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
  def putIfNew(tx: Transaction): TracedResult[ValidationError, Boolean]
  def removeAll(txs: Traversable[Transaction]): Unit
  def transactionById(transactionId: ByteStr): Option[Transaction]
  def packUnconfirmed(rest: MultiDimensionalMiningConstraint, maxPackTime: Duration): (Seq[Transaction], MultiDimensionalMiningConstraint)

  def spendableBalance(addr: Address, assetId: Asset): Long
  def pessimisticPortfolio(addr: Address): Portfolio

  def transactions: Seq[Transaction]
  def size: Int
}
