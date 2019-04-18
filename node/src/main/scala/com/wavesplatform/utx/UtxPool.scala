package com.wavesplatform.utx

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.state.Portfolio
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.smart.script.trace.TracedResult

trait UtxPool extends AutoCloseable {
  self =>

  def putIfNewTraced(tx: Transaction): TracedResult[ValidationError, Boolean]

  def putIfNew(tx: Transaction): Either[ValidationError, Boolean] = putIfNewTraced(tx).resultE

  def removeAll(txs: Traversable[Transaction]): Unit

  def spendableBalance(addr: Address, assetId: Asset): Long

  def pessimisticPortfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint)

}
